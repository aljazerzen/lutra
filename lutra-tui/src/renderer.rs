use std::io::{self, Write};

use crossterm::cursor;
use crossterm::queue;
use crossterm::style::{
    Attribute, Print, ResetColor, SetAttribute, SetBackgroundColor, SetForegroundColor,
};
use crossterm::terminal::{self, Clear, ClearType};
use unicode_segmentation::UnicodeSegmentation;
use unicode_width::UnicodeWidthStr;

use crate::terminal::Rect;
use crate::terminal::{Line, Span, View};

pub(crate) struct ShellRenderer {
    stdout: io::Stdout,
    /// Absolute row where the live region starts (right after committed content).
    region_start_row: u16,
    /// Height of the live region from the last render.
    last_region_height: u16,
}

impl ShellRenderer {
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

    // print before the "working" region
    pub fn print(&mut self, view: View<'_>) -> io::Result<()> {
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

    // render the "working" region
    pub(crate) fn render(&mut self, view: View, area: Rect) -> io::Result<()> {
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
            self.render_line(start + i as u16, line, area.cols)?;
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

    fn render_line(&mut self, row: u16, line: &Line<'_>, cols: u16) -> io::Result<()> {
        queue!(
            self.stdout,
            cursor::MoveTo(0, row),
            Clear(ClearType::CurrentLine)
        )?;
        self.queue_line(&truncate_line(line, cols))?;
        queue!(self.stdout, ResetColor)?;
        Ok(())
    }

    fn queue_line(&mut self, line: &Line<'_>) -> io::Result<()> {
        for span in &line.spans {
            self.queue_span(span)?;
        }
        queue!(self.stdout, ResetColor)?;
        Ok(())
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

fn truncate_line(line: &Line<'_>, width: u16) -> Line<'static> {
    if width == 0 {
        return Line::default();
    }

    let max = width as usize;
    let mut out = Line::default();
    let mut used = 0usize;

    for span in &line.spans {
        let mut chunk = String::new();
        for grapheme in span.content.graphemes(true) {
            let grapheme_width = UnicodeWidthStr::width(grapheme);
            if grapheme_width > 0 && used + grapheme_width > max {
                break;
            }
            chunk.push_str(grapheme);
            used += grapheme_width;
        }
        if !chunk.is_empty() {
            out.push_span(Span::new(chunk).set(span.style));
        }
        if used >= max {
            break;
        }
    }

    out
}
