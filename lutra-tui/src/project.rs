use std::path::PathBuf;
use std::rc::Rc;

use lutra_compiler::error::{DiagnosticMessage, Error as CompileError};
use lutra_compiler::{CheckParams, Project, SourceTree, check};

/// Centralized state holding compilation results.
pub struct ProjectState {
    /// Path to the project root.
    pub path: PathBuf,

    /// Last compilation result.
    pub compilation: CompileResult,

    /// Compilation in progress.
    pub compiling: bool,
}

/// Result of compiling a project.
pub enum CompileResult {
    /// Compilation succeeded.
    Success { project: Box<Project> },
    /// Compilation failed with diagnostics.
    Failed {
        diagnostics: Rc<Vec<DiagnosticMessage>>,
    },
    /// Not yet compiled.
    Pending,
}

impl ProjectState {
    /// Creates a new project state for the given path.
    pub fn new(project_path: PathBuf) -> Self {
        Self {
            path: project_path,
            compilation: CompileResult::Pending,
            compiling: false,
        }
    }

    /// Recompiles the project from the current source tree.
    pub fn recompile(&mut self) {
        self.compiling = true;

        // Discover
        let source = match discover(self.path.clone()) {
            Ok(s) => s,
            Err(e) => {
                self.compiling = false;
                self.compilation = CompileResult::Failed {
                    diagnostics: Rc::new(vec![]),
                };
                eprintln!("Failed to load sources: {e}");
                return;
            }
        };

        // Check
        self.compilation = match check(source, CheckParams {}) {
            Ok(project) => CompileResult::Success {
                project: Box::new(project),
            },
            Err(CompileError::Compile { diagnostics }) => CompileResult::Failed {
                diagnostics: Rc::new(diagnostics),
            },
            Err(e) => {
                // Log the error for now - in the future we might want to display it
                eprintln!("Compilation error: {e}");
                // Convert other errors to a diagnostic-like message
                CompileResult::Failed {
                    diagnostics: Rc::new(vec![]),
                }
            }
        };
        self.compiling = false;
    }

    /// Returns the number of errors in the current compilation.
    pub fn diagnostic_count(&self) -> usize {
        match &self.compilation {
            CompileResult::Success { .. } => 0,
            CompileResult::Failed { diagnostics } => diagnostics.len(),
            CompileResult::Pending => 0,
        }
    }
}

/// Loads all .lt files from a project directory into a SourceTree.
fn discover(path: PathBuf) -> Result<SourceTree, lutra_compiler::error::Error> {
    let params = lutra_compiler::DiscoverParams {
        project: Some(path),
    };
    lutra_compiler::discover(params)
}
