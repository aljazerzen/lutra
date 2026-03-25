use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::mpsc::{Receiver, Sender, channel};
use std::thread;
use std::time::Duration;

use notify::{RecursiveMode, Watcher};

use crate::terminal::Action;

/// Watches a project directory for .lt file changes with smart debouncing.
///
/// The watcher:
/// 1. Discovers the initial SourceTree
/// 2. Watches only directories containing .lt files
/// 3. Accumulates file change events and debounces them over 100ms
/// 4. Emits a single SourceUpdated action for all accumulated changes
pub struct FileWatcher {
    _watcher_thread: Option<thread::JoinHandle<()>>,
}

impl FileWatcher {
    /// Creates a new file watcher for the given project path.
    ///
    /// Immediately performs initial discovery and sends SourceUpdated action.
    /// Then watches only directories that contain .lt files.
    pub fn new(project_path: PathBuf, action_tx: Sender<Action>) -> anyhow::Result<Self> {
        let init_thread = thread::spawn(move || {
            // Initial discovery
            match discover_and_watch(project_path, action_tx) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("Failed to initialize file watcher: {e}");
                }
            }
        });

        Ok(Self {
            _watcher_thread: Some(init_thread),
        })
    }
}

/// Discover the project and set up file watching.
/// Returns the initial SourceTree for testing/debugging purposes.
fn discover_and_watch(project_path: PathBuf, action_tx: Sender<Action>) -> anyhow::Result<()> {
    // Initial discovery
    let source_tree = lutra_compiler::discover(lutra_compiler::DiscoverParams {
        project: Some(project_path.clone()),
    })?;

    // Send initial SourceUpdated event
    let _ = action_tx.send(Action::SourceUpdated(source_tree.clone()));

    // Extract parent directories to watch
    let watch_dirs = get_watch_dirs(&source_tree);

    // Create file watcher
    let (event_tx, event_rx) = channel();
    let mut watcher = notify::recommended_watcher(move |result| {
        match result {
            Ok(event) => {
                // Only care about changes to .lt files
                if event_is_relevant(&event) {
                    let _ = event_tx.send(());
                }
            }
            Err(e) => {
                eprintln!("Watch error: {e}");
            }
        }
    })?;

    update_watched_dirs(&mut watcher, &watch_dirs, &Default::default());

    // Debounce thread: accumulates events for 100ms and triggers rediscovery
    thread::spawn(move || {
        let mut watcher = watcher;
        let mut watch_dirs = watch_dirs;

        debounce_loop(event_rx, || {
            // Rediscover the project
            match lutra_compiler::discover(lutra_compiler::DiscoverParams {
                project: Some(project_path.clone()),
            }) {
                Ok(source_tree) => {
                    // Get new set of watch directories
                    let new_watch_dirs = get_watch_dirs(&source_tree);

                    update_watched_dirs(&mut watcher, &new_watch_dirs, &watch_dirs);
                    watch_dirs = new_watch_dirs;

                    // Send the updated SourceTree (single event for all changes)
                    let _ = action_tx.send(Action::SourceUpdated(source_tree));
                }
                Err(e) => {
                    eprintln!("Failed to discover project after file changes: {e}");
                    // Keep watching with current directories and wait for next change
                }
            }
        });
    });

    Ok(())
}

fn update_watched_dirs(
    watcher: &mut impl Watcher,
    new_dirs: &HashSet<PathBuf>,
    old_dirs: &HashSet<PathBuf>,
) {
    for dir in new_dirs.iter() {
        if !old_dirs.contains(dir)
            && let Err(e) = watcher.watch(dir, RecursiveMode::NonRecursive)
        {
            eprintln!("Failed to watch new directory {:?}: {e}", dir);
        }
    }

    for dir in old_dirs.iter() {
        if !new_dirs.contains(dir)
            && let Err(e) = watcher.unwatch(dir)
        {
            eprintln!("Failed to unwatch directory {:?}: {e}", dir);
        }
    }
}

/// Main debounce loop: accumulates file change events
fn debounce_loop(event_rx: Receiver<()>, mut f: impl FnMut()) {
    // wait for an event (blocking)
    while event_rx.recv().is_ok() {
        // wait until 100ms of silence
        loop {
            match event_rx.recv_timeout(Duration::from_millis(100)) {
                Ok(_) => {
                    // another event came in, keep waiting
                }
                Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                    // 100ms of silence, trigger rediscovery
                    break;
                }
                Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                    // channel closed, exit
                    return;
                }
            }
        }

        f()
    }

    // channel closed
}

/// Extract all unique parent directories from a SourceTree.
fn get_watch_dirs(source_tree: &lutra_compiler::SourceTree) -> HashSet<PathBuf> {
    let mut dirs = HashSet::new();

    for (path, _) in source_tree.get_sources() {
        // Convert relative path to absolute
        let abs_path = source_tree.get_absolute_path(path);

        // Get parent directory
        if let Some(parent) = abs_path.parent() {
            dirs.insert(parent.to_path_buf());
        }
    }

    dirs
}

/// Check if a file event is relevant (involves .lt files).
fn event_is_relevant(event: &notify::Event) -> bool {
    event.paths.iter().any(|p| is_lutra_file(p))
}

/// Returns true if the path is a Lutra source file (.lt extension).
fn is_lutra_file(path: &Path) -> bool {
    path.extension().is_some_and(|ext| ext == "lt")
}
