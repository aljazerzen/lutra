use std::collections::HashMap;
use std::str::FromStr;
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock};
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::*;

use lutra_compiler::{SourceTree, codespan, pr};

#[derive(clap::Parser)]
pub struct Command {
    #[clap(flatten)]
    transport: Transport,
}

#[derive(clap::Args)]
#[group(required = true, multiple = false)]
struct Transport {
    /// Use stdin/stdout transport
    #[clap(long)]
    stdio: bool,
}

#[tokio::main(flavor = "current_thread")]
pub async fn run(_cmd: Command) {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp_server::LspService::new(Backend::new);
    tower_lsp_server::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}

#[derive(Debug)]
struct Backend {
    client: tower_lsp_server::Client,

    /// Documents currently "open" by the LSP client.
    /// Typically, these are files open in the client editor.
    documents: Arc<Mutex<HashMap<Uri, TextDocumentItem>>>,

    #[allow(dead_code)]
    projects: Arc<RwLock<Vec<Project>>>,
}

#[derive(Debug)]
#[allow(dead_code)]
struct Project {
    root_path: Option<std::path::PathBuf>,

    /// The whole source tree, including files that are not "open" by LSP client
    source_tree: Arc<Mutex<SourceTree>>,

    /// Parsed AST of the project
    ast: Arc<Mutex<pr::ModuleDef>>,

    /// Checked project
    project: Arc<Mutex<Option<lutra_compiler::Project>>>,
}

impl Backend {
    fn new(client: tower_lsp_server::Client) -> Self {
        Backend {
            client,
            documents: Default::default(),
            projects: Default::default(),
        }
    }
}

impl tower_lsp_server::LanguageServer for Backend {
    async fn initialize(&self, _p: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                // lutra_compiler::codespan::line_numbers uses UTF16 columns
                position_encoding: Some(PositionEncodingKind::UTF16),

                // TODO: support for incremental sync
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                document_formatting_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },

            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) -> () {
        let mut docs = self.documents.lock().await;

        let doc = params.text_document;
        docs.insert(doc.uri.clone(), doc.clone());

        self.check_document(doc).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) -> () {
        let mut docs = self.documents.lock().await;
        docs.remove(&params.text_document.uri);
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) -> () {
        let mut docs = self.documents.lock().await;

        if let Some(document) = docs.get_mut(&params.text_document.uri) {
            if document.version < params.text_document.version {
                // we only support FULL document text sync,
                // so there will always be exactly one change with a full content updated
                let change = params.content_changes.get_mut(0).unwrap();
                assert!(change.range.is_none());

                document.text = std::mem::take(&mut change.text);
                document.version = params.text_document.version;

                self.check_document(document.clone()).await;
            } else {
                tracing::warn!("document version out of sync!")
            }
        }
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let doc = {
            let docs = self.documents.lock().await;
            let Some(doc) = docs.get(&params.text_document.uri) else {
                return Ok(None);
            };
            doc.clone()
        };

        let source_tree = document_to_source_tree(doc);

        let (err, edits) = lutra_compiler::format(&source_tree);
        if err.is_some() {
            // don't format syntactically invalid documents
            return Ok(None);
        }

        let (_, source) = source_tree.get_sources().next().unwrap();

        let edits = codespan::minimize_text_edits(source, edits);

        Ok(Some(convert_text_edits(source, edits)))
    }
}

impl Backend {
    async fn check_document(&self, document: TextDocumentItem) {
        let uri = document.uri.clone();
        let version = document.version;

        let source_tree = document_to_source_tree(document);

        let res = lutra_compiler::check(source_tree, Default::default());
        let diagnostics = match res {
            Ok(_project) => {
                // maybe save the project?
                vec![]
            }
            Err(err) => match err {
                lutra_compiler::error::Error::Compile { diagnostics } => diagnostics,
                _ => {
                    self.client.show_message(MessageType::ERROR, err).await;
                    vec![]
                }
            },
        };

        let diags = diagnostics.into_iter().map(convert_diagnostic).collect();
        self.client
            .publish_diagnostics(uri, diags, Some(version))
            .await;
    }
}

fn document_to_source_tree(doc: TextDocumentItem) -> SourceTree {
    let path = std::path::PathBuf::from_str(doc.uri.path().as_str()).unwrap();

    lutra_compiler::SourceTree::single(path, doc.text)
}

fn convert_range(r: &codespan::Range) -> Range {
    Range {
        start: convert_position(&r.start),
        end: convert_position(&r.end),
    }
}

fn convert_position(lc: &codespan::LineColumn) -> Position {
    Position {
        line: lc.line,
        character: lc.column,
    }
}

fn convert_diagnostic(d: lutra_compiler::error::DiagnosticMessage) -> Diagnostic {
    Diagnostic {
        message: d.message().to_string(),
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String(d.code().to_string())),
        range: convert_range(d.range()),
        ..Default::default()
    }
}

fn convert_text_edits(source: &str, edits: Vec<codespan::TextEdit>) -> Vec<TextEdit> {
    let line_numbers = codespan::LineNumbers::new(source);

    let mut r = Vec::with_capacity(edits.len());
    for edit in edits {
        let range = line_numbers.range_of_span(edit.span);
        r.push(TextEdit {
            range: convert_range(&range),
            new_text: edit.new_text,
        });
    }
    r
}
