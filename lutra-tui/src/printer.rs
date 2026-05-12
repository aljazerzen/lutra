use std::io::{self, Write};

use crossterm::cursor;
use crossterm::queue;
use crossterm::style::{
    Attribute, Print, ResetColor, SetAttribute, SetBackgroundColor, SetForegroundColor,
};
use crossterm::terminal::{self, Clear, ClearType};

use crate::terminal::Rect;
use crate::terminal::{Line, Span, View};

/// Abstracts over the real terminal and a recording surface used in tests.
pub(crate) trait Printer {
    fn commit_view(&mut self, view: View<'_>) -> io::Result<()>;
    fn update_view(&mut self, view: View<'_>, area: Rect) -> io::Result<()>;
}

// ---------------------------------------------------------------------------
// TerminalPrinter — writes to the real terminal
// ---------------------------------------------------------------------------

pub(crate) struct TerminalPrinter {
    stdout: io::Stdout,
    /// Absolute row where the live region starts (right after committed content).
    region_start_row: u16,
    /// Height of the live region from the last render.
    last_region_height: u16,
}

impl TerminalPrinter {
    pub(crate) fn new() -> io::Result<Self> {
        terminal::enable_raw_mode()?;
        let mut stdout = io::stdout();
        queue!(stdout, cursor::Show)?;
        stdout.flush()?;

        let (_, row) = cursor::position().unwrap_or((0, 0));

        Ok(Self {
            stdout,
            region_start_row: row,
            last_region_height: 0,
        })
    }

    pub(crate) fn restore(&mut self) -> io::Result<()> {
        let (_, rows) = terminal::size().unwrap_or((0, 1));
        let region_end = self
            .region_start_row
            .saturating_add(self.last_region_height)
            .min(rows.saturating_sub(1));
        queue!(
            self.stdout,
            cursor::MoveTo(0, region_end),
            ResetColor,
            Print("\r\n"),
            cursor::Show
        )?;
        self.stdout.flush()?;
        terminal::disable_raw_mode()
    }
}

impl Printer for TerminalPrinter {
    fn commit_view(&mut self, view: View<'_>) -> io::Result<()> {
        if view.is_empty() {
            return Ok(());
        }

        let (_cols, rows) = terminal::size()?;

        self.region_start_row = self.region_start_row.min(rows);

        if self.region_start_row >= rows {
            queue!(
                self.stdout,
                cursor::MoveTo(0, rows.saturating_sub(1)),
                Print("\r\n")
            )?;
            self.region_start_row = rows.saturating_sub(1);
        }

        let start = self.region_start_row;
        queue!(
            self.stdout,
            cursor::MoveTo(0, start),
            Clear(ClearType::FromCursorDown)
        )?;

        for (i, line) in view.lines.iter().enumerate() {
            if i > 0 {
                queue!(self.stdout, Print("\r\n"))?;
            }
            self.queue_line(line)?;
        }

        self.stdout.flush()?;
        self.region_start_row = start.saturating_add(view.line_count() as u16).min(rows);
        self.last_region_height = 0;
        Ok(())
    }

    fn update_view(&mut self, view: View, area: Rect) -> io::Result<()> {
        let mut lines = view.lines;
        if lines.is_empty() {
            lines.push(Line::default());
        }

        self.region_start_row = self.region_start_row.min(area.rows);

        let visible_height = (lines.len() as u16).min(area.rows.max(1));
        let available = area.rows.saturating_sub(self.region_start_row);

        if visible_height > available {
            let scroll = visible_height - available;
            queue!(self.stdout, cursor::MoveTo(0, area.rows.saturating_sub(1)))?;
            for _ in 0..scroll {
                queue!(self.stdout, Print("\r\n"))?;
            }
            self.region_start_row = self.region_start_row.saturating_sub(scroll);
        }

        let start = self.region_start_row;
        queue!(
            self.stdout,
            cursor::MoveTo(0, start),
            Clear(ClearType::FromCursorDown)
        )?;

        let skip = lines.len().saturating_sub(visible_height as usize);
        let visible_lines = &lines[skip..];

        for (i, line) in visible_lines.iter().enumerate() {
            queue!(
                self.stdout,
                cursor::MoveTo(0, start + i as u16),
                Clear(ClearType::CurrentLine)
            )?;
            let mut l = line.to_owned();
            l.split_at_width(area.cols as usize);
            self.queue_line(&l)?;
        }

        let cursor = view.cursor.unwrap_or((0, 0));
        let cursor_line_idx = cursor
            .0
            .saturating_sub(skip)
            .min(visible_height.saturating_sub(1) as usize);
        let cursor_row = start + cursor_line_idx as u16;
        let cursor_col = cursor.1.min(area.cols.saturating_sub(1));

        queue!(
            self.stdout,
            cursor::MoveTo(cursor_col, cursor_row),
            cursor::Show
        )?;
        self.stdout.flush()?;

        self.last_region_height = visible_height;
        Ok(())
    }
}

impl TerminalPrinter {
    fn queue_line(&mut self, line: &Line<'_>) -> io::Result<()> {
        for span in &line.spans {
            self.queue_span(span)?;
        }
        queue!(self.stdout, ResetColor)
    }

    fn queue_span(&mut self, span: &Span<'_>) -> io::Result<()> {
        if let Some(fg) = span.style.fg {
            queue!(self.stdout, SetForegroundColor(fg))?;
        }
        if let Some(bg) = span.style.bg {
            queue!(self.stdout, SetBackgroundColor(bg))?;
        }
        if span.style.bold {
            queue!(self.stdout, SetAttribute(Attribute::Bold))?;
        }
        if span.style.underline {
            queue!(self.stdout, SetAttribute(Attribute::Underlined))?;
        }
        queue!(self.stdout, Print(&span.content))?;
        let has_style = span.style.fg.is_some()
            || span.style.bg.is_some()
            || span.style.bold
            || span.style.underline;
        if has_style {
            queue!(self.stdout, SetAttribute(Attribute::Reset))?;
        }
        Ok(())
    }
}
