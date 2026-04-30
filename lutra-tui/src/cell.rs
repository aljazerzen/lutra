use std::time::Duration;

use crate::input;
use crate::repl::{Cursor, OutputCursor};
use crate::table;
use crate::terminal::{Line, Rect, Span, Style, View};
use lutra_bin::rr::{self, ProgramType};

/// A single committed REPL history entry.
pub struct Cell {
    pub program: String,

    // Type of the program. Set after compilation.
    pub program_ty: Option<rr::ProgramType>,

    pub input: Option<input::InputPane>,

    pub output: Option<CellOutput>,
}

/// Focused section within a frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CellStage {
    Browse,
    Program,
    Input,
    Output,
}

/// Output state of a REPL cell.
pub enum CellOutput {
    CompileError(String),
    RunError(String),
    RunOutput {
        data: Vec<u8>,
        layout: Option<table::Layout>,
        duration: Duration,
    },
    Message(View<'static>),
}

impl Cell {
    pub fn new() -> Self {
        Cell {
            program: String::new(),
            program_ty: None,
            input: None,
            output: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.program.is_empty() && self.is_input_empty() && self.is_output_empty()
    }

    pub fn is_input_empty(&self) -> bool {
        self.input.is_none()
    }

    pub fn is_output_empty(&self) -> bool {
        self.output.is_none()
    }

    pub fn output_layout(&self) -> Option<&table::Layout> {
        match &self.output {
            Some(CellOutput::RunOutput { layout, .. }) => layout.as_ref(),
            _ => None,
        }
    }

    pub fn get_input_bin(&self) -> Option<Vec<u8>> {
        let (Some(input), Some(program_ty)) = (&self.input, &self.program_ty) else {
            return None;
        };
        Some(input.get_value_bin(&program_ty.input, &program_ty.defs))
    }

    pub fn render<'a>(
        &'a self,
        area: Rect,
        cursor: Option<&Cursor>,
        completions: View<'a>,
    ) -> View<'a> {
        if area.rows == 0 {
            return View::new();
        }

        let mut view = View::new();

        let gutter = Span::styled(
            "▌ ",
            match &self.output {
                None => Style::new(),
                Some(CellOutput::Message(_)) => Style::info(),
                Some(CellOutput::RunOutput { .. }) => Style::success(),
                Some(CellOutput::CompileError(_) | CellOutput::RunError(_)) => Style::danger(),
            },
        );

        // program
        view.push_line(Line::new(&self.program));
        if let Some(c) = cursor
            && c.on_program()
        {
            let line = view.lines.last_mut().unwrap();
            let span = line.spans.last_mut().unwrap();
            let after = span.split_at(c.program_col);
            view.set_cursor_inline();
            view.lines.last_mut().unwrap().push_span(after);
        }
        if cursor.is_some_and(Cursor::is_browsing) {
            view.set_cursor_inline();
        }
        view.extend_lines(completions);

        if let Some(input) = &self.input {
            view.push_line(Line::empty());
            let focused = cursor.is_some_and(Cursor::on_input);
            view.extend_lines(input.render(focused).to_owned());
        }

        if let Some(output) = &self.output {
            view.push_line(Line::empty());
            if !matches!(output, CellOutput::Message(_)) {
                view.push_line(Line::styled(
                    output.signature(self.program_ty.as_ref()),
                    Style::muted(),
                ));
            }

            let height = (area.rows as usize)
                .saturating_sub(view.line_count() + 2)
                .min(30);
            let (mut output, visible_lines) =
                output.render(height, cursor.map(|c| &c.output), self.program_ty.as_ref());
            cursor.inspect(|c| c.page_size.set(visible_lines));

            if !cursor.is_some_and(Cursor::on_output) {
                output.cursor = None;
            }
            output.truncate(height as usize);
            view.extend_lines(output);
        }

        view.prefix(gutter);
        view.wrap(area.cols, 1);
        view
    }
}

impl CellOutput {
    fn signature(&self, ty: Option<&ProgramType>) -> String {
        let mut parts: Vec<String> = Vec::with_capacity(4);
        parts.push("output".into());
        if let CellOutput::RunOutput { data, duration, .. } = self {
            if let Some(ty) = ty {
                let shape = lutra_bin::get_shape(data, &ty.output, &ty.defs);
                if let Some(n) = shape.items {
                    parts.push(format!("{n} items"));
                }
                if let Some(n) = shape.fields {
                    parts.push(format!("{n} fields"));
                }
            }
            parts.push(format!("{duration:.2?}"));
        }
        parts.join(" · ")
    }

    fn render<'a>(
        &'a self,
        height: usize,
        cursor: Option<&OutputCursor>,
        ty: Option<&'a ProgramType>,
    ) -> (View<'a>, usize) {
        match self {
            CellOutput::CompileError(error) | CellOutput::RunError(error) => {
                let text = if error.is_empty() {
                    "error"
                } else {
                    error.as_str()
                };
                let scroll = cursor.and_then(|c| c.as_plain()).unwrap_or_default();
                let view = View::from(
                    text.lines()
                        .skip(scroll)
                        .take(height)
                        .map(str::to_string)
                        .map(Line::new)
                        .collect::<Vec<_>>(),
                );
                let visible_lines = view.line_count();
                (view, visible_lines)
            }
            CellOutput::RunOutput { data, layout, .. } => {
                let Some(ty) = ty else {
                    return (View::new(), 0);
                };

                if let Some(layout) = layout {
                    let t_cur = cursor.and_then(|c| c.as_table());
                    crate::table::Table::new(data, &ty.output, &ty.defs)
                        .render(layout, height, t_cur)
                } else {
                    let scroll = cursor.and_then(|c| c.as_plain()).unwrap_or_default();
                    let view = View::from(
                        lutra_bin::print_source(data, &ty.output, &ty.defs)
                            .unwrap_or_else(|_| "<output>".to_string())
                            .lines()
                            .skip(scroll)
                            .take(height)
                            .map(str::to_string)
                            .map(Line::new)
                            .collect::<Vec<_>>(),
                    );
                    let visible_lines = view.line_count();
                    (view, visible_lines)
                }
            }
            CellOutput::Message(message) => {
                let mut view = View::new();
                view.set_cursor_here(0);
                view.extend_lines(message.clone());
                (view, 0)
            }
        }
    }
}
