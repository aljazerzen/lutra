use std::rc::Rc;
use std::time::Duration;

use crate::input;
use crate::repl::{Cursor, OutputCursor};
use crate::table;
use crate::terminal::{Line, Rect, Span, Style, View};
use lutra_bin::rr::{self, ProgramType};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoundInput {
    pub data: Rc<Vec<u8>>,
    pub ty_source: String,
}

/// A single committed REPL history entry.
pub struct Cell {
    pub prompt: String,

    // Type of the program. Set after compilation.
    pub program_ty: Option<rr::ProgramType>,

    pub argument: Option<input::InputPane>,

    pub bound_input: Option<BoundInput>,

    pub output: Option<CellOutput>,
}

/// Focused section within a frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CellStage {
    Browse,
    Program,
    Argument,
    Output,
}

/// Output state of a REPL cell.
pub enum CellOutput {
    CompileError(String),
    RunError(String),
    RunOutput {
        data: Rc<Vec<u8>>,
        layout: Option<table::Layout>,
        duration: Duration,
    },
    Message(View<'static>),
}

pub struct CellOutputRef<'a> {
    pub cell: &'a Cell,
    pub data: &'a Rc<Vec<u8>>,
    #[allow(dead_code)]
    pub duration: &'a Duration,
}

impl Cell {
    pub fn new() -> Self {
        Cell {
            prompt: String::new(),
            program_ty: None,
            argument: None,
            bound_input: None,
            output: None,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.prompt.is_empty()
            && self.is_argument_empty()
            && self.is_bound_input_empty()
            && self.is_output_empty()
    }

    pub fn is_argument_empty(&self) -> bool {
        self.argument.is_none()
    }

    pub fn is_bound_input_empty(&self) -> bool {
        self.bound_input.is_none()
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

    pub fn as_output(&self) -> Option<CellOutputRef<'_>> {
        let Some(CellOutput::RunOutput { data, duration, .. }) = &self.output else {
            return None;
        };
        Some(CellOutputRef {
            cell: self,
            data,
            duration,
        })
    }

    pub fn get_argument_bin(&self) -> Option<Vec<u8>> {
        let (Some(argument), Some(program_ty)) = (&self.argument, &self.program_ty) else {
            return None;
        };
        Some(argument.get_value_bin(&program_ty.input, &program_ty.defs))
    }

    fn get_prompt_header(&self) -> Option<String> {
        self.bound_input
            .as_ref()
            .map(|i| format!("func (x: {}) ->", i.ty_source))
    }

    pub fn get_program_source(&self) -> String {
        self.get_prompt_header()
            .map(|h| h + "\n")
            .unwrap_or_default()
            + &self.prompt
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

        if let Some(header) = self.get_prompt_header() {
            view.push_line(Line::new(header));
        }

        // prompt
        view.push_line(Line::new(&self.prompt));
        if let Some(c) = cursor
            && c.on_program()
        {
            let line = view.lines.last_mut().unwrap();
            let span = line.spans.last_mut().unwrap();
            let after = span.split_at(c.prompt_col);
            view.set_cursor_inline();
            view.lines.last_mut().unwrap().push_span(after);
        }
        if cursor.is_some_and(Cursor::is_browsing) {
            view.set_cursor_inline();
        }
        view.extend_lines(completions);

        if self.bound_input.is_none()
            && let Some(argument) = &self.argument
        {
            view.push_line(Line::empty());
            let focused = cursor.is_some_and(Cursor::on_argument);
            view.extend_lines(argument.render(focused).to_owned());
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
            output.truncate(height);
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

impl<'a> CellOutputRef<'a> {
    pub fn as_bound_input(&self) -> BoundInput {
        let data = Rc::clone(self.data); // TODO: make this Rc::clone
        let ty = self.cell.program_ty.as_ref().unwrap();
        let ty_source = lutra_bin::ir::print_ty(&ty.output);
        BoundInput { data, ty_source }
    }
}
