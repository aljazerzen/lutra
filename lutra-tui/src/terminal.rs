use std::{borrow::Cow, fmt::Write};

use crossterm::event;
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

pub use crossterm::style::Color;

/// Top-level component of a TUI.
///
/// Other components should always have similar functions for rendering and handling actions,
/// but with additional parameters (additional state from the parent).
pub trait Component {
    fn handle(&mut self, action: Action) -> ActionResult;
}

#[derive(Debug)]
pub enum Action {
    /// Terminal event (raw, uninterpreted)
    Terminal(event::Event),

    /// Runner responses
    RunnerMessage(lutra_runner::proto::Response),

    /// Source tree updated from file watcher
    SourceUpdated(lutra_compiler::SourceTree),

    // App-level commands (high-level)
    Exit,
    ExitIfEmpty,
    SubmitPrompt,
    SubmitInput,

    // REPL navigation
    HistoryUp,
    HistoryDown,
    HistorySelect,
    CycleFocusNext,
    CycleFocusPrev,
    ClearCell,

    // diagnostics
    ScrollDiagnosticsUp,
    ScrollDiagnosticsDown,

    // completions
    CompletionUp,
    CompletionDown,
    CompletionApply,
    CompletionHide,

    // text editing
    EditMoveBackward,
    EditMoveForward,
    EditMoveWordBackward,
    EditMoveWordForward,
    EditMoveHome,
    EditMoveEnd,
    EditDeleteBackward,
    EditDeleteWordBackward,
    EditDeleteForward,
    EditDeleteWordForward,

    // output table navigation
    TableMoveUp,
    TableMoveDown,
    TableMoveLeft,
    TableMoveRight,
    TableMoveHome,
    TableMoveEnd,
    TableMovePageUp,
    TableMovePageDown,
}

impl Action {
    /// Extract a key event from a Terminal action.
    pub fn as_key(&self) -> Option<event::KeyEvent> {
        match self {
            Action::Terminal(event::Event::Key(key)) => Some(*key),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Effect {
    /// Request a redraw.
    Redraw,
    /// Request app shutdown.
    Shutdown,
    /// Commit styled lines above the current inline viewport.
    Print { view: View<'static> },
}

#[derive(Default)]
#[must_use]
pub struct ActionResult {
    actions: Vec<Action>,
    effects: Vec<Effect>,
}

impl ActionResult {
    pub fn redraw() -> Self {
        Self {
            effects: vec![Effect::Redraw],
            ..Default::default()
        }
    }
    pub fn shutdown() -> Self {
        Self {
            effects: vec![Effect::Shutdown],
            ..Default::default()
        }
    }
    pub fn action(action: Action) -> Self {
        Self {
            actions: vec![action],
            ..Default::default()
        }
    }
    pub fn print(view: View<'static>) -> Self {
        Self {
            effects: vec![Effect::Print { view }, Effect::Redraw],
            ..Default::default()
        }
    }

    pub(crate) fn into_parts(self) -> (Vec<Action>, Vec<Effect>) {
        (self.actions, self.effects)
    }

    pub(crate) fn and(mut self, other: Self) -> Self {
        self.actions.extend(other.actions);
        self.effects.extend(other.effects);
        self
    }
}

#[derive(Clone, Copy)]
pub struct Rect {
    pub rows: u16,
    pub cols: u16,
}

impl Rect {
    #[allow(dead_code)]
    pub fn split_top(self, rows_top: u16) -> (Self, Self) {
        let top = Rect {
            rows: rows_top.min(self.rows),
            cols: self.cols,
        };
        let bottom = Rect {
            rows: self.rows.saturating_sub(top.rows),
            cols: self.cols,
        };
        (top, bottom)
    }
    pub fn split_bottom(self, rows_bottom: u16) -> (Self, Self) {
        let bottom = Rect {
            rows: rows_bottom.min(self.rows),
            cols: self.cols,
        };
        let top = Rect {
            rows: self.rows.saturating_sub(bottom.rows),
            cols: self.cols,
        };
        (top, bottom)
    }
}

impl From<(u16, u16)> for Rect {
    fn from((cols, rows): (u16, u16)) -> Self {
        Rect { rows, cols }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct View<'a> {
    pub lines: Vec<Line<'a>>,
    pub cursor: Option<(usize, u16)>,
}

impl<'a> View<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_text(text: &'a str) -> Self {
        View::from(text.lines().map(Line::from).collect::<Vec<_>>())
    }

    #[allow(dead_code)]
    pub fn with_cursor(mut self, cursor: (usize, u16)) -> Self {
        self.cursor = Some(cursor);
        self
    }

    pub fn set_cursor_inline(&mut self) {
        let line = self.lines.len().saturating_sub(1);
        let col = self.lines.last().map_or(0, |l| l.width());
        self.cursor = Some((line, col as u16));
    }

    pub fn set_cursor_here(&mut self, col: u16) {
        let line = self.lines.len();
        self.cursor = Some((line, col));
    }

    pub fn push_line(&mut self, line: impl Into<Line<'a>>) {
        self.lines.push(line.into());
    }

    pub fn push_span(&mut self, span: impl Into<Span<'a>>) {
        if self.lines.is_empty() {
            self.lines.push(Line::empty());
        }
        self.lines.last_mut().unwrap().push_span(span);
    }

    pub fn extend_lines(&mut self, other: Self) {
        let other_cursor = other.cursor.map(|(l, c)| (l + self.line_count(), c));
        self.cursor = self.cursor.or(other_cursor);
        self.lines.extend(other.lines);
    }

    pub fn truncate(&mut self, height: usize) {
        self.lines.truncate(height);
        if let Some(c) = &mut self.cursor {
            c.0 = c.0.min(height.saturating_sub(1));
        }
    }

    pub fn line_count(&self) -> usize {
        self.lines.len()
    }

    pub fn is_empty(&self) -> bool {
        self.lines.is_empty()
    }

    /// Convert any lifetime to `'static` by cloning borrowed content.
    pub fn to_owned(&self) -> View<'static> {
        View {
            lines: self.lines.iter().map(Line::to_owned).collect(),
            cursor: self.cursor,
        }
    }

    /// Insert `prefix` in front of every line.
    pub fn prefix(&mut self, prefix: Span<'a>) {
        for line in &mut self.lines {
            line.spans.insert(0, prefix.clone());
        }
        if let Some((_, c)) = &mut self.cursor {
            *c += prefix.width() as u16;
        }
    }

    /// Insert `prefix` in front of the first line.
    pub fn prefix_first(&mut self, prefix: Span<'a>) {
        if let Some(line) = self.lines.first_mut() {
            line.spans.insert(0, prefix.clone());
        }
        if let Some((l, c)) = &mut self.cursor
            && *l == 0
        {
            *c += prefix.width() as u16;
        }
    }

    /// Wrap lines that exceed `cols` columns.
    ///
    /// `prefix` is the number of leading spans on each line that act as a
    /// margin: they are excluded from wrapping and prepended verbatim to every
    /// continuation line produced when a line is split.
    pub fn wrap(&mut self, cols: u16, prefix: usize) {
        let mut result: Vec<Line<'a>> = Vec::with_capacity(self.lines.len());

        for mut line in std::mem::take(&mut self.lines) {
            let content = line.spans.split_off(prefix.min(line.spans.len()));
            let prefix = line.spans;

            let prefix_width: u16 = prefix.iter().map(|s| s.width() as u16).sum();
            let content_cols = cols.saturating_sub(prefix_width);
            let content_line = Line { spans: content };

            if content_line.width() as u16 <= content_cols {
                let mut reassembled = Line { spans: prefix };
                reassembled.spans.extend(content_line.spans);
                result.push(reassembled);
                continue;
            }

            let mut wrapped = View::new();
            wrap_line(&mut wrapped, &content_line, content_cols);
            for wrapped_line in wrapped.lines {
                let mut prefixed = Line {
                    spans: prefix.clone(),
                };
                prefixed.spans.extend(wrapped_line.spans);
                result.push(prefixed);
            }
        }

        self.lines = result;
    }
}

impl<'a> From<Vec<Line<'a>>> for View<'a> {
    fn from(lines: Vec<Line<'a>>) -> Self {
        Self {
            lines,
            cursor: None,
        }
    }
}
impl<'a> From<Line<'a>> for View<'a> {
    fn from(line: Line<'a>) -> Self {
        Self {
            lines: vec![line],
            cursor: None,
        }
    }
}

impl<'a> std::fmt::Display for View<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for l in &self.lines {
            f.write_str(l.to_string().trim_end())?;
            f.write_char('\n')?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Line<'a> {
    pub spans: Vec<Span<'a>>,
}

impl<'a> Line<'a> {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn new(content: impl Into<Cow<'a, str>>) -> Self {
        Self {
            spans: vec![Span::new(content)],
        }
    }

    pub fn styled(content: impl Into<Cow<'a, str>>, style: Style) -> Self {
        Self {
            spans: vec![Span::styled(content, style)],
        }
    }

    pub fn push_span(&mut self, span: impl Into<Span<'a>>) {
        self.spans.push(span.into());
    }

    pub fn push_string(&mut self, s: String) {
        self.spans.push(s.into());
    }

    pub fn extend(&mut self, line: Line<'a>) {
        self.spans.extend(line.spans);
    }

    pub fn width(&self) -> usize {
        self.spans.iter().map(Span::width).sum()
    }

    pub fn is_empty(&self) -> bool {
        self.spans.iter().all(Span::is_empty)
    }

    /// Splits the line at a certain character width.
    /// Returns the new line and remaining width on the first line.
    pub fn split_at_width(&mut self, width: usize) -> (Option<Line<'_>>, usize) {
        let mut split = None;
        let mut rem_width = width;
        for (i, s) in self.spans.iter_mut().enumerate() {
            let (new_s, rem) = s.split_at_width(rem_width);
            rem_width = rem;
            if let Some(new_s) = new_s {
                split = Some((i, new_s));
                break;
            }
        }
        if let Some((last_idx, first_span)) = split {
            let mut new_line = Vec::with_capacity(self.spans.len() - last_idx);
            new_line.push(first_span);
            new_line.extend(self.spans.drain((last_idx + 1)..));
            (Some(Line::from(new_line)), rem_width)
        } else {
            (None, rem_width)
        }
    }

    /// Map a function over all spans, producing a new line.
    pub fn map_spans(mut self, mut f: impl FnMut(Span<'a>) -> Span<'a>) -> Self {
        for span in &mut self.spans {
            let owned = std::mem::replace(span, Span::new(""));
            *span = f(owned);
        }
        self
    }

    /// Convert any lifetime to `'static` by cloning borrowed content.
    pub fn to_owned(&self) -> Line<'static> {
        Line {
            spans: self.spans.iter().map(Span::to_owned).collect(),
        }
    }
}

impl<'a> From<&'a str> for Line<'a> {
    fn from(value: &'a str) -> Self {
        Self::new(value)
    }
}

impl From<String> for Line<'static> {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl<'a> From<Vec<Span<'a>>> for Line<'a> {
    fn from(spans: Vec<Span<'a>>) -> Self {
        Self { spans }
    }
}

impl<'a> From<Span<'a>> for Line<'a> {
    fn from(span: Span<'a>) -> Self {
        Self { spans: vec![span] }
    }
}

impl<'a> std::fmt::Display for Line<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for s in &self.spans {
            f.write_str(&s.content)?
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span<'a> {
    pub content: Cow<'a, str>,
    pub style: Style,
}

impl<'a> Span<'a> {
    pub fn new(content: impl Into<Cow<'a, str>>) -> Self {
        Self {
            content: content.into(),
            style: Style::default(),
        }
    }

    pub fn styled(content: impl Into<Cow<'a, str>>, style: Style) -> Self {
        Self {
            content: content.into(),
            style,
        }
    }

    pub fn set(mut self, style: Style) -> Self {
        self.style = style;
        self
    }

    pub fn split_at_width(&mut self, width: usize) -> (Option<Span<'a>>, usize) {
        let mut rem = width;
        for (i, g) in self.content.grapheme_indices(true) {
            let w = UnicodeWidthStr::width(g);
            if w > rem {
                // split here
                let r = self.split_at(i);
                return (Some(r), rem);
            }
            rem -= w;
        }
        (None, rem)
    }

    /// Convert any lifetime to `'static` by cloning any borrowed content.
    pub fn to_owned(&self) -> Span<'static> {
        Span {
            content: Cow::Owned(self.content.clone().into_owned()),
            style: self.style,
        }
    }

    pub fn width(&self) -> usize {
        self.content
            .graphemes(true)
            .map(UnicodeWidthStr::width)
            .sum()
    }

    pub fn split_at(&mut self, i: usize) -> Span<'a> {
        match &mut self.content {
            Cow::Borrowed(s) => {
                let (l, r) = s.split_at(i);
                *s = l;
                Span::styled(r, self.style)
            }
            Cow::Owned(s) => {
                let r = s.split_off(i);
                Span::styled(r, self.style)
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.content.is_empty()
    }
}

impl<'a> From<&'a str> for Span<'a> {
    fn from(value: &'a str) -> Self {
        Self::new(value)
    }
}

impl From<String> for Span<'static> {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Style {
    pub fg: Option<Color>,
    pub bg: Option<Color>,
    pub bold: bool,
    pub underline: bool,
}

#[allow(dead_code)]
impl Style {
    pub const fn new() -> Self {
        Self {
            fg: None,
            bg: None,
            bold: false,
            underline: false,
        }
    }

    pub const fn fg(mut self, color: Color) -> Self {
        self.fg = Some(color);
        self
    }

    pub const fn bg(mut self, color: Color) -> Self {
        self.bg = Some(color);
        self
    }

    pub const fn bold(mut self) -> Self {
        self.bold = true;
        self
    }

    pub const fn underline(mut self) -> Self {
        self.underline = true;
        self
    }
    pub fn patch(self, other: Style) -> Self {
        Self {
            fg: other.fg.or(self.fg),
            bg: other.bg.or(self.bg),
            bold: self.bold || other.bold,
            underline: self.underline || other.underline,
        }
    }

    pub const fn muted() -> Self {
        Self::new().fg(Color::DarkGrey)
    }
    pub const fn cursor() -> Self {
        Self::new().bg(Color::Grey).fg(Color::Black)
    }
    pub const fn cursor_pale() -> Self {
        Self::new().bg(Color::DarkGrey)
    }
    pub const fn accent() -> Self {
        Self::new().fg(Color::Rgb {
            r: 0x4b,
            g: 0x65,
            b: 0x7b,
        })
    }
    pub const fn accent2() -> Self {
        Self::new().fg(Color::Rgb {
            r: 0x6d,
            g: 0xa0,
            b: 0xca,
        })
    }
    pub const fn success() -> Self {
        Self::new().fg(Color::Green)
    }
    pub const fn warning() -> Self {
        Self::new().fg(Color::Yellow)
    }
    pub const fn magic() -> Self {
        Self::new().fg(Color::Magenta)
    }
    pub const fn info() -> Self {
        Self::new().fg(Color::Blue)
    }
    pub const fn danger() -> Self {
        Self::new().fg(Color::Red)
    }
}

pub fn wrap_line<'a>(out: &mut View<'a>, line: &Line<'a>, width: u16) {
    if width == 0 || line.spans.is_empty() {
        return;
    }

    let max = width as usize;
    let mut current = Line::default();
    let mut current_width = 0usize;
    let mut pushed = false;

    for span in &line.spans {
        let mut chunk = String::new();
        let mut chunk_width = 0usize;

        for grapheme in span.content.graphemes(true) {
            let grapheme_width = UnicodeWidthStr::width(grapheme);
            if current_width > 0 && grapheme_width > 0 && current_width + grapheme_width > max {
                if !chunk.is_empty() {
                    current.push_span(Span::new(std::mem::take(&mut chunk)).set(span.style));
                    chunk_width = 0;
                }
                out.push_line(current);
                pushed = true;
                current = Line::default();
                current_width = 0;
            }
            chunk.push_str(grapheme);
            chunk_width += grapheme_width;
        }

        if !chunk.is_empty() {
            current.push_span(Span::new(chunk).set(span.style));
            current_width += chunk_width;
        }
    }
    let _ = current_width;

    if !pushed || !current.is_empty() {
        out.push_line(current);
    }
}

/// Spawns a background thread that reads terminal events and sends them as actions.
///
/// The thread will exit when the channel is closed (receiver dropped) or when
/// an error occurs reading events (e.g., terminal closed).
pub(super) fn spawn_event_reader(
    action_tx: std::sync::mpsc::Sender<Action>,
) -> std::thread::JoinHandle<()> {
    std::thread::Builder::new()
        .name("event-reader".to_string())
        .spawn(move || {
            loop {
                // This blocks until an event arrives
                match event::read() {
                    Ok(event) => {
                        // Send terminal event as an action
                        if action_tx.send(Action::Terminal(event)).is_err() {
                            // Channel closed, exit thread
                            break;
                        }
                    }
                    Err(e) => {
                        // Error reading event (terminal closed/corrupted?)
                        eprintln!("Event reader error: {e}");
                        break;
                    }
                }
            }
        })
        .expect("Failed to spawn event reader thread")
}
