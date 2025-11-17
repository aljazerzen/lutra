use lutra_compiler::DiscoverParams;
use std::collections::HashMap;
use std::path;
use std::sync::Arc;
use tokio::sync::{Mutex, RwLock};
use tower_lsp_server::UriExt;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::lsp_types::*;

use lutra_compiler::SourceTree;
use lutra_compiler::codespan;

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

    /// Projects that the LS is aware of.
    #[allow(dead_code)]
    projects: RwLock<Vec<Arc<Mutex<Project>>>>,
}

#[derive(Debug)]
#[allow(dead_code)]
struct Project {
    /// The whole source tree, including files that are not "open" by LSP client
    source_tree: SourceTree,

    source_id_to_uri: HashMap<u16, Uri>,

    /// Checked project
    checked: Option<lutra_compiler::Project>,
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

                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                        change: Some(TextDocumentSyncKind::INCREMENTAL),
                        will_save: Some(false),
                        will_save_wait_until: Some(false),
                    },
                )),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),

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
        let uri = params.text_document.uri.clone();
        {
            let mut docs_lock = self.documents.lock().await;

            let doc = params.text_document;
            docs_lock.insert(doc.uri.clone(), doc);
        }

        self.check_document(uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) -> () {
        let mut docs = self.documents.lock().await;
        docs.remove(&params.text_document.uri);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) -> () {
        let mut to_check = None;

        {
            let mut docs_lock = self.documents.lock().await;
            if let Some(document) = docs_lock.get_mut(&params.text_document.uri) {
                if document.version < params.text_document.version {
                    apply_changes(&mut document.text, params.content_changes);
                    document.version = params.text_document.version;

                    to_check = Some(document.uri.clone())
                } else {
                    tracing::warn!("document version out of sync!");
                }
            }
        }

        if let Some(uri) = to_check {
            self.check_document(uri).await;
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

        let line_index = codespan::LineNumbers::new(source);
        Ok(Some(to_proto::text_edits(&line_index, edits)))
    }

    async fn range_formatting(
        &self,
        params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let mut doc = {
            let docs = self.documents.lock().await;
            let Some(doc) = docs.get(&params.text_document.uri) else {
                return Ok(None);
            };
            doc.clone()
        };

        let range = from_proto::range(&params.range);

        // extract the range to format
        let index = codespan::LineNumbers::new(&doc.text);
        let span = index.span_of_range(&range, 1);
        doc.text = span.take_slice(doc.text);

        // format
        // TODO: this does not work if the range spans only a part of a def,
        // even if it spans a valid expression.
        // We should probably parse the whole document, discard any errors out
        // of formatting range, extract the AST nodes that cover the selection
        // and format them.
        let source_tree = document_to_source_tree(doc);

        let (err, edits) = lutra_compiler::format(&source_tree);
        if err.is_some() {
            // don't format syntactically invalid documents
            return Ok(None);
        }

        // offset edits into index space of the original document
        let edits = codespan::offset_text_edits(edits, span.start as i32);

        Ok(Some(to_proto::text_edits(&index, edits)))
    }
}

impl Backend {
    async fn check_document(&self, uri: Uri) {
        let path = from_proto::path(&uri);

        // find project
        let mut project = self.find_project_of_doc(&path).await;
        if project.is_none() {
            project = self.discover_project(path.clone()).await;
        }
        let Some(project) = project else {
            return;
        };

        let mut project_lock = project.lock().await;

        // update source tree
        {
            let r_path = project_lock.source_tree.get_relative_path(&path).unwrap();
            let documents_lock = self.documents.lock().await;
            if let Some(doc) = documents_lock.get(&uri) {
                let r = project_lock.source_tree.replace(r_path, doc.text.clone());
                assert!(r.is_some());
            }
        }

        // check
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "checking project: {}",
                    debug_project_paths(&project_lock.source_tree)
                ),
            )
            .await;
        let res = lutra_compiler::check(project_lock.source_tree.clone(), Default::default());
        let diagnostics = match res {
            Ok(p) => {
                project_lock.checked = Some(p);

                vec![]
            }
            Err(lutra_compiler::error::Error::Compile { diagnostics }) => diagnostics,
            Err(err) => {
                self.client.show_message(MessageType::ERROR, err).await;
                vec![]
            }
        };
        self.client
            .log_message(
                MessageType::INFO,
                format!(".. {} diagnostics", diagnostics.len()),
            )
            .await;

        // publish diagnostics
        let mut diagnostics_by_source: HashMap<u16, Vec<_>> = HashMap::new();
        for d in diagnostics {
            let Some(span) = d.span() else {
                continue;
            };
            let by_source = diagnostics_by_source.entry(span.source_id).or_default();
            by_source.push(d);
        }

        let documents_lock = self.documents.lock().await;

        for source_id in project_lock.source_tree.get_source_ids() {
            let diagnostics = diagnostics_by_source.remove(source_id).unwrap_or_default();

            let uri = source_id_to_uri(&project_lock.source_tree, source_id);

            let doc = documents_lock.get(&uri);
            let version = doc.map(|d| d.version);

            let diags = diagnostics.into_iter().map(to_proto::diagnostic).collect();
            self.client.publish_diagnostics(uri, diags, version).await;
        }
    }

    async fn find_project_of_doc(&self, path: &path::Path) -> Option<Arc<Mutex<Project>>> {
        let projects = self.projects.read().await;

        for p in projects.iter() {
            let p_lock = p.lock().await;

            let Ok(r_path) = p_lock.source_tree.get_relative_path(path) else {
                continue;
            };

            if p_lock.source_tree.get_source(r_path).is_some() {
                return Some(Arc::clone(p));
            }
        }
        None
    }

    async fn discover_project(&self, path: path::PathBuf) -> Option<Arc<Mutex<Project>>> {
        self.client
            .log_message(
                MessageType::INFO,
                format!("discovering project: {}", path.display()),
            )
            .await;

        let res = tokio::task::spawn_blocking(|| {
            lutra_compiler::discover(DiscoverParams {
                project: Some(path),
            })
        })
        .await
        .ok()?;

        let mut source_tree = match res {
            Ok(s) => s,
            Err(e) => {
                self.client
                    .log_message(MessageType::ERROR, e.to_string())
                    .await;
                return None;
            }
        };

        {
            let documents_lock = self.documents.lock().await;
            let source_ids: Vec<_> = source_tree.get_source_ids().cloned().collect();
            for source_id in source_ids {
                let uri = source_id_to_uri(&source_tree, &source_id);
                if let Some(doc) = documents_lock.get(&uri) {
                    self.client
                        .log_message(MessageType::INFO, format!("overlaying doc: {}", *doc.uri))
                        .await;

                    let path = source_tree.get_path(source_id).unwrap().to_path_buf();
                    source_tree.replace(&path, doc.text.clone());
                }
            }
        }

        self.client
            .log_message(
                MessageType::INFO,
                format!("discovered project: {}", debug_project_paths(&source_tree)),
            )
            .await;

        let project = Arc::new(Mutex::new(Project {
            source_tree,
            source_id_to_uri: Default::default(),
            checked: None,
        }));

        let mut projects_lock = self.projects.write().await;
        projects_lock.push(project.clone());

        Some(project)
    }
}

fn debug_project_paths(source_tree: &SourceTree) -> String {
    let paths_debug: Vec<_> = source_tree
        .get_sources()
        .map(|(p, _)| p.display().to_string())
        .collect();
    paths_debug.join(", ")
}

fn source_id_to_uri(source_tree: &SourceTree, source_id: &u16) -> Uri {
    // TODO: this feels wrong, we should rethink identifiers in SourceTree

    let path = source_tree.get_path(*source_id).unwrap();
    let path = source_tree.get_absolute_path(path);
    Uri::from_file_path(path).unwrap()
}

fn document_to_source_tree(doc: TextDocumentItem) -> SourceTree {
    lutra_compiler::SourceTree::single(from_proto::path(&doc.uri), doc.text)
}

fn apply_changes(text: &mut String, changes: Vec<TextDocumentContentChangeEvent>) {
    // find last full update
    let full_replace = changes.iter().rposition(|c| c.range.is_none());

    let mut changes = changes.into_iter().skip(full_replace.unwrap_or_default());
    if full_replace.is_some() {
        let full_replace = changes.next().unwrap();
        *text = full_replace.text;
    };

    // The changes we got must be applied sequentially, but can cross lines so we
    // have to keep our line index updated.
    // Some clients (e.g. Code) sort the ranges in reverse. As an optimization, we
    // remember the last valid line in the index and only rebuild it if needed.
    let mut line_numbers = codespan::LineNumbers::new(text);
    let mut index_valid = u32::MAX;

    for change in changes {
        let range = from_proto::range(change.range.as_ref().unwrap());
        if range.end.line >= index_valid {
            line_numbers = codespan::LineNumbers::new(text);
        }
        index_valid = range.start.line;

        let span = line_numbers.span_of_range(&range, 0);

        *text = codespan::apply_text_edits(
            text,
            &[codespan::TextEdit {
                span,
                new_text: change.text,
            }],
        );
    }
}

mod to_proto {
    use lutra_compiler::codespan;
    use tower_lsp_server::lsp_types::*;

    pub fn range(r: &codespan::Range) -> Range {
        Range {
            start: line_col(&r.start),
            end: line_col(&r.end),
        }
    }
    pub fn line_col(lc: &codespan::LineColumn) -> Position {
        Position {
            line: lc.line,
            character: lc.column,
        }
    }

    pub fn diagnostic(d: lutra_compiler::error::DiagnosticMessage) -> Diagnostic {
        Diagnostic {
            message: d.message().to_string(),
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String(d.code().to_string())),
            range: range(d.range()),
            ..Default::default()
        }
    }

    pub fn text_edits(
        line_index: &codespan::LineNumbers,
        edits: Vec<codespan::TextEdit>,
    ) -> Vec<TextEdit> {
        let mut res = Vec::with_capacity(edits.len());
        for edit in edits {
            let rng = line_index.range_of_span(edit.span);
            res.push(TextEdit {
                range: range(&rng),
                new_text: edit.new_text,
            });
        }
        res
    }
}

mod from_proto {
    use lutra_compiler::codespan;
    use tower_lsp_server::{UriExt, lsp_types::*};

    pub fn path(uri: &Uri) -> std::path::PathBuf {
        uri.to_file_path().unwrap().into_owned()
    }
    pub fn range(r: &Range) -> codespan::Range {
        codespan::Range {
            start: line_col(&r.start),
            end: line_col(&r.end),
        }
    }
    pub fn line_col(p: &Position) -> codespan::LineColumn {
        codespan::LineColumn {
            line: p.line,
            column: p.character,
        }
    }
}
