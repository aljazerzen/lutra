#![cfg(test)]

use std::io;

use crossterm::event::{Event, KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};

use crate::RunnerConfig;
use crate::terminal::{Action, Rect, View};
use crate::{driver, printer, runner, shell};

/// Deterministic test entrypoint: replays `events` against a fresh shell with
/// an empty project and a [`RecordingPrinter`], then returns all captured views.
///
/// Unlike [`run_shell`] this function:
/// - does not touch the real terminal
/// - does not spawn a file-watcher or terminal event-reader thread
/// - processes events synchronously, draining runner messages after each one
pub fn record_shell(
    runner_cfg: runner::RunnerConfig,
    runner: lutra_runner::channel::Client,
    runner_thread: std::thread::JoinHandle<()>,
    viewport: Rect,
    events: Vec<crossterm::event::Event>,
) -> anyhow::Result<String> {
    let (action_tx, action_rx) = std::sync::mpsc::channel();

    // real runner, no file watcher
    let runner = runner::RunnerProxy::try_new(runner, runner_thread, action_tx)?;

    let mut driver = driver::Driver {
        app: shell::Shell::new(None, runner_cfg, runner.get_client()),
        printer: RecordingPrinter::default(),
        viewport,
        queue: Default::default(),
    };

    // start
    driver.start()?;

    // initial source update (normally sent by FileWatcher)
    if driver.step(Action::SourceUpdated(lutra_compiler::SourceTree::empty()))? {
        return Ok(driver.printer.finish());
    }
    if sleep_and_drain(&mut driver, &action_rx)? {
        return Ok(driver.printer.finish());
    }

    // replay each scripted terminal event
    for event in events {
        if driver.step(Action::Terminal(event))? {
            break;
        }
        if sleep_and_drain(&mut driver, &action_rx)? {
            break;
        }
    }

    drop(runner);
    Ok(driver.printer.finish())
}

fn sleep_and_drain<P: printer::Printer>(
    driver: &mut driver::Driver<P>,
    action_rx: &std::sync::mpsc::Receiver<Action>,
) -> io::Result<bool> {
    // Give the runner / message-forwarding threads a window to respond.
    std::thread::sleep(std::time::Duration::from_millis(10));
    loop {
        match action_rx.try_recv() {
            Ok(action) => {
                if driver.step(action)? {
                    return Ok(true);
                }
            }
            Err(_) => return Ok(false),
        }
    }
}

// ---------------------------------------------------------------------------
// RecordingPrinter — captures views for deterministic tests
// ---------------------------------------------------------------------------

/// A no-op printer that records every view it receives.
///
/// `print()` calls accumulate into [`printed`], and each `render()` call
/// overwrites [`last_rendered`].  `restore()` is a no-op.
#[derive(Default)]
pub(crate) struct RecordingPrinter {
    pub committed: Vec<View<'static>>,
    pub live: Option<View<'static>>,
}

impl printer::Printer for RecordingPrinter {
    fn commit_view(&mut self, view: View<'_>) -> io::Result<()> {
        self.committed.push(view.to_owned());
        Ok(())
    }
    fn update_view(&mut self, view: View<'_>, _area: Rect) -> io::Result<()> {
        self.live = Some(view.to_owned());
        Ok(())
    }
}

impl RecordingPrinter {
    /// Format all captured views as a deterministic snapshot string.
    ///
    /// Uses [`Display`] (plain text, no ANSI) for readability.  Appends cursor
    /// position when present.  Strips two sources of non-determinism:
    ///
    /// - Status-bar key-event debug suffix (`⸱ KeyEvent { … }`)
    /// - Wall-clock durations on output-signature lines (`· 10.09ms`)
    pub fn finish(self) -> String {
        let mut out = String::new();
        for v in &self.committed {
            out += &v.to_string();
        }
        if let Some(v) = &self.live {
            out += &v.to_string();
            if let Some((row, col)) = v.cursor {
                out += &format!("[cursor {row},{col}]");
            }
        }
        normalize(out)
    }
}

/// Strip dynamic content that would make snapshots fragile:
/// - The status bar appends `⸱ {key:?}` for the last key pressed.
/// - Output-signature lines contain a wall-clock duration (`output · 1.23ms`).
fn normalize(s: String) -> String {
    // U+2E31: status-bar segment separator; U+00B7: output-signature separator.
    const KEY_MARKER: &str = " \u{2e31} KeyEvent { ";
    const SEG: &str = " \u{00b7} ";
    let lines: Vec<String> = s
        .lines()
        .map(|line| {
            let line = match line.find(KEY_MARKER) {
                Some(pos) => &line[..pos],
                None => line,
            };
            if let Some(pos) = line.rfind(SEG) {
                let tail = &line[pos + SEG.len()..];
                if is_duration(tail) {
                    return line[..pos].to_string();
                }
            }
            line.to_string()
        })
        .collect();
    let mut joined = lines.join("\n");
    if s.ends_with('\n') {
        joined.push('\n');
    }
    joined
}

fn is_duration(s: &str) -> bool {
    for suffix in ["\u{b5}s", "ms", "ns", "s"] {
        if let Some(rest) = s.strip_suffix(suffix) {
            return !rest.is_empty() && rest.chars().all(|c| c.is_ascii_digit() || c == '.');
        }
    }
    false
}

// ---------------------------------------------------------------------------
// Runner factories
// ---------------------------------------------------------------------------

fn interpreter_runner() -> (lutra_runner::channel::Client, std::thread::JoinHandle<()>) {
    let runner = lutra_interpreter::InterpreterRunner::default();
    let runner = lutra_runner::AsyncRunner::new(runner);
    let (client, server) = lutra_runner::channel::new_pair(runner);
    let handle = std::thread::spawn(move || {
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(server.listen());
    });
    (client, handle)
}

/// A mock runner that always fails on execute with a known error message.
struct AlwaysErrorRunner;

impl lutra_runner::Run for AlwaysErrorRunner {
    async fn prepare(&self, _: lutra_bin::rr::Program) -> Result<u32, lutra_runner::proto::Error> {
        Ok(0)
    }

    async fn execute(
        &self,
        _: u32,
        _input: &[u8],
    ) -> Result<lutra_bin::vec::Vec<u8>, lutra_runner::proto::Error> {
        Err(lutra_runner::proto::Error {
            display: "mock execution error".to_string(),
            code: None,
        })
    }

    async fn release(&self, _: u32) -> Result<(), lutra_runner::proto::Error> {
        Ok(())
    }
}

fn error_runner() -> (lutra_runner::channel::Client, std::thread::JoinHandle<()>) {
    let (client, server) = lutra_runner::channel::new_pair(AlwaysErrorRunner);
    let handle = std::thread::spawn(move || {
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(server.listen());
    });
    (client, handle)
}

/// A mock runner that returns a fixed schema string on `/pull` and succeeds on
/// all other operations.
struct FixedSchemaRunner {
    schema: &'static str,
}

impl lutra_runner::Run for FixedSchemaRunner {
    async fn prepare(&self, _: lutra_bin::rr::Program) -> Result<u32, lutra_runner::proto::Error> {
        Ok(0)
    }

    async fn execute(
        &self,
        _: u32,
        _input: &[u8],
    ) -> Result<lutra_bin::vec::Vec<u8>, lutra_runner::proto::Error> {
        Ok(lutra_bin::vec::Vec::new())
    }

    async fn release(&self, _: u32) -> Result<(), lutra_runner::proto::Error> {
        Ok(())
    }

    async fn pull_schema(&self) -> Result<lutra_bin::string::String, lutra_runner::proto::Error> {
        Ok(self.schema.to_string())
    }
}

fn schema_runner(
    schema: &'static str,
) -> (lutra_runner::channel::Client, std::thread::JoinHandle<()>) {
    let (client, server) = lutra_runner::channel::new_pair(FixedSchemaRunner { schema });
    let handle = std::thread::spawn(move || {
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
            .block_on(server.listen());
    });
    (client, handle)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn key(code: KeyCode) -> Event {
    Event::Key(KeyEvent {
        code,
        modifiers: KeyModifiers::NONE,
        kind: KeyEventKind::Press,
        state: KeyEventState::NONE,
    })
}

fn enter() -> Event {
    key(KeyCode::Enter)
}

fn esc() -> Event {
    key(KeyCode::Esc)
}

fn type_str(s: &str) -> Vec<Event> {
    s.chars().map(|c| key(KeyCode::Char(c))).collect()
}

fn cfg() -> RunnerConfig {
    RunnerConfig {
        repr: lutra_compiler::ProgramRepr::BytecodeLt,
    }
}

fn area() -> Rect {
    Rect { cols: 80, rows: 24 }
}

fn record_with(
    runner: (lutra_runner::channel::Client, std::thread::JoinHandle<()>),
    events: Vec<Event>,
) -> String {
    record_shell(cfg(), runner.0, runner.1, area(), events).unwrap()
}

fn record(events: Vec<Event>) -> String {
    record_with(interpreter_runner(), events)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[test]
fn startup_empty_project() {
    insta::assert_snapshot!(record(vec![]), @"
    ▌ Lutra v0.5.1
    ▌ Tip:  Enter to run  ·  ↑↓ for history  ·  Esc to clear  ·  Ctrl+Q to exit

    ▌ project
    ▌
    ▌ module
    ▌ m std

    ▌
    ────────────────────
     ⸱ ok ⸱ BytecodeLt ⸱ Cell(Program)
    [cursor 0,2]
    ");
}

#[test]
fn help_command() {
    let mut events = type_str("/help");
    events.push(enter());
    insta::assert_snapshot!(record(events), @"
    ▌ Lutra v0.5.1
    ▌ Tip:  Enter to run  ·  ↑↓ for history  ·  Esc to clear  ·  Ctrl+Q to exit

    ▌ project
    ▌
    ▌ module
    ▌ m std

    ▌ /help
    ▌
    ▌ Available commands
    ▌
    ▌ /pull    Fetch schema and rewrite @schema module
    ▌ /export  Export the last successful result
    ▌ /help    Show help
    ▌ /clear   Clear history and release memory
    ▌ /quit    Quit (aliases: /q, /exit)

    ▌
    ────────────────────
     ⸱ ok ⸱ BytecodeLt ⸱ Cell(Program)
    [cursor 0,2]
    ");
}

#[test]
fn typing_in_prompt() {
    insta::assert_snapshot!(record(type_str("hello world")), @"
    ▌ Lutra v0.5.1
    ▌ Tip:  Enter to run  ·  ↑↓ for history  ·  Esc to clear  ·  Ctrl+Q to exit

    ▌ project
    ▌
    ▌ module
    ▌ m std

    ▌ hello world
    ────────────────────
     ⸱ ok ⸱ BytecodeLt ⸱ Cell(Program)
    [cursor 0,13]
    ");
}

/// Press Esc after execution to commit the result cell.
#[test]
fn run_success() {
    let mut events = type_str("\"hello\"");
    events.push(enter());
    events.push(esc());
    insta::assert_snapshot!(record(events), @r#"
    ▌ Lutra v0.5.1
    ▌ Tip:  Enter to run  ·  ↑↓ for history  ·  Esc to clear  ·  Ctrl+Q to exit

    ▌ project
    ▌
    ▌ module
    ▌ m std

    ▌ "hello"
    ▌
    ▌ output
    ▌ value
    ▌ text
    ▌ ─────
    ▌ hello

    ▌
    ────────────────────
     ⸱ ok ⸱ BytecodeLt ⸱ Cell(Program)
    [cursor 0,2]
    "#);
}

#[test]
fn run_error() {
    let mut events = type_str("\"hello\"");
    events.push(enter());
    events.push(esc());
    insta::assert_snapshot!(record_with(error_runner(), events), @r#"
    ▌ Lutra v0.5.1
    ▌ Tip:  Enter to run  ·  ↑↓ for history  ·  Esc to clear  ·  Ctrl+Q to exit

    ▌ project
    ▌
    ▌ module
    ▌ m std

    ▌ "hello"
    ▌
    ▌ output
    ▌ mock execution error

    ▌
    ────────────────────
     ⸱ ok ⸱ BytecodeLt ⸱ Cell(Program)
    [cursor 0,2]
    "#);
}

#[test]
fn pull_schema() {
    let schema = "let greeting: text";
    let mut events = type_str("/pull");
    events.push(enter());
    insta::assert_snapshot!(record_with(schema_runner(schema), events), @"
    ▌ Lutra v0.5.1
    ▌ Tip:  Enter to run  ·  ↑↓ for history  ·  Esc to clear  ·  Ctrl+Q to exit

    ▌ project
    ▌
    ▌ module
    ▌ m std

    ▌ /pull
    ▌
    ▌ let greeting: text

    ▌
    ────────────────────
     ⸱ ok ⸱ BytecodeLt ⸱ Cell(Program)
    [cursor 0,2]
    ");
}

#[test]
fn resize() {
    let mut events = type_str("hello world");
    events.push(Event::Resize(40, 12));
    insta::assert_snapshot!(record(events), @"
    ▌ Lutra v0.5.1
    ▌ Tip:  Enter to run  ·  ↑↓ for history  ·  Esc to clear  ·  Ctrl+Q to exit

    ▌ project
    ▌
    ▌ module
    ▌ m std

    ▌ hello world
    ────────────────────
     ⸱ ok ⸱ BytecodeLt ⸱ Cell(Program)
    [cursor 0,13]
    ");
}
