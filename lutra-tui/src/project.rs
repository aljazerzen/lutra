use std::rc::Rc;

use lutra_compiler::error::{DiagnosticMessage, Error as CompileError};
use lutra_compiler::{CheckParams, Project, SourceTree, check};

/// Centralized state holding compilation results.
pub struct ProjectState {
    /// Last source code.
    pub source: Option<SourceTree>,

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
    pub fn new() -> Self {
        Self {
            source: None,
            compilation: CompileResult::Pending,
            compiling: false,
        }
    }

    /// Compiles the project with a provided source tree (from the file watcher).
    pub fn recompile(&mut self) {
        let Some(source) = self.source.clone() else {
            return;
        };

        self.compiling = true;

        // Check (skip discover, use provided source)
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
