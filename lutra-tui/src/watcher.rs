use std::path::{Path, PathBuf};
use std::sync::mpsc::Sender;
use std::time::Duration;

use notify::RecursiveMode;
use notify_debouncer_mini::{DebouncedEventKind, Debouncer, new_debouncer};

use crate::terminal::Action;

/// Watches a project directory for .lt file changes with debouncing.
pub struct FileWatcher {
    _debouncer: Debouncer<notify::RecommendedWatcher>,
}

impl FileWatcher {
    /// Creates a new file watcher for the given project path.
    ///
    /// The watcher monitors for `.lt` file changes and sends `Action::Recompile`
    /// through the provided channel with 100ms debouncing.
    pub fn new(project_path: PathBuf, tx: Sender<Action>) -> anyhow::Result<Self> {
        let debouncer = new_debouncer(
            Duration::from_millis(100),
            move |result: Result<Vec<notify_debouncer_mini::DebouncedEvent>, notify::Error>| {
                match result {
                    Ok(events) => {
                        for event in events {
                            if matches!(event.kind, DebouncedEventKind::Any)
                                && is_lutra_file(&event.path)
                            {
                                let _ = tx.send(Action::Recompile);
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!("File watch error: {e}");
                    }
                }
            },
        )?;

        let mut watcher = Self {
            _debouncer: debouncer,
        };
        watcher.watch(&project_path)?;

        Ok(watcher)
    }

    fn watch(&mut self, path: &Path) -> anyhow::Result<()> {
        self._debouncer
            .watcher()
            .watch(path, RecursiveMode::Recursive)?;
        Ok(())
    }
}

/// Returns true if the path is a Lutra source file (.lt extension).
fn is_lutra_file(path: &Path) -> bool {
    path.extension().is_some_and(|ext| ext == "lt")
}
