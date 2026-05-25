use std::rc::Rc;
use std::time::Duration;

use crossterm::event::KeyCode;

use crate::cell::{Cell, CellOutput, CellOutputRef, CellStage};
use crate::completions::Completions;
use crate::editor::{self, Edit};
use crate::terminal::{Action, ActionResult, View};
use crate::{input, table};

/// REPL pane for evaluating arbitrary expressions.
pub struct ReplPane {
    /// Current cell that is being edited.
    pub(crate) draft: Cell,

    /// Current live cell state.
    pub(crate) cursor: Cursor,

    /// Completion suggestions state.
    pub completions: Completions,

    /// Expression history snapshots.
    history: Vec<Rc<Cell>>,

    /// Committed cells waiting to be rendered and printed by the shell driver.
    pub(crate) pending_prints: Vec<Rc<Cell>>,
}

impl ReplPane {
    /// Creates a new REPL pane.
    pub fn new() -> Self {
        Self {
            draft: Cell::new(),
            cursor: Cursor::new(),
            completions: Completions::new(),
            history: Vec::new(),
            pending_prints: Vec::new(),
        }
    }

    pub fn visible_cell(&self) -> &Cell {
        if let Some(c) = self.cursor.history_cursor.and_then(|c| self.history.get(c)) {
            c
        } else {
            &self.draft
        }
    }

    pub fn cursor_and_cell<T>(&mut self, f: impl FnOnce(&mut Cursor, &Cell) -> T) -> T {
        let history_idx = self.cursor.history_cursor;
        let cell: &Cell = history_idx
            .and_then(|i| self.history.get(i))
            .map(Rc::as_ref)
            .unwrap_or(&self.draft);
        f(&mut self.cursor, cell)
    }

    pub fn checkout_from_history(&mut self) -> ActionResult {
        // if cell has been executed, commit
        let res = if self.draft.is_output_empty() {
            ActionResult::default()
        } else {
            self.commit_cell(true)
        };

        // If looking at history cell, copy it into draft
        if self.cursor.history_cursor.is_some() {
            let mut new_cell = Cell::new();
            let history_cell = self.visible_cell();
            new_cell.prompt = history_cell.prompt.clone();
            new_cell.program_ty = history_cell.program_ty.clone();
            new_cell.argument = history_cell.argument.clone();
            new_cell.bound_input = history_cell.bound_input.clone();

            self.draft = new_cell;
            self.cursor.goto_draft(Some(self.draft.prompt.len()));
        }

        res
    }

    /// Commit the draft cell: moves it to history.
    pub fn commit_cell(&mut self, keep_prompt: bool) -> ActionResult {
        if self.draft.is_empty() {
            return ActionResult::default();
        }
        // take draft
        let cell = std::mem::replace(&mut self.draft, Cell::new());
        if keep_prompt {
            self.draft.prompt = cell.prompt.clone();
        }
        self.cursor.goto_draft(Some(self.draft.prompt.len()));
        self.completions.update_query(&self.draft.prompt);

        let rc = Rc::new(cell);
        self.history.push(Rc::clone(&rc));
        self.pending_prints.push(rc);
        ActionResult::redraw()
    }

    /// Build a run error entry from the current cell state.
    pub fn commit_error(&mut self, message: impl Into<String>) -> ActionResult {
        self.draft.output = Some(CellOutput::RunError(message.into()));
        self.commit_cell(false)
    }

    /// Build a compile error entry from the current cell state.
    pub fn commit_compile_error(&mut self, message: impl Into<String>) -> ActionResult {
        self.draft.output = Some(CellOutput::CompileError(message.into()));
        self.commit_cell(false)
    }

    /// Commit a message-only cell from the current draft state.
    pub fn commit_message(&mut self, message: View<'static>) -> ActionResult {
        self.draft.output = Some(CellOutput::Message(message));
        self.commit_cell(false)
    }

    pub fn edit(&mut self, make_edit: impl FnOnce(&str, usize) -> Edit) -> ActionResult {
        let res = self.checkout_from_history();

        self.draft.program_ty = None;
        self.draft.argument = None;
        self.draft.output = None;

        let edit = make_edit(&self.draft.prompt, self.cursor.prompt_col);
        edit.apply(&mut self.draft.prompt, &mut self.cursor.prompt_col);

        self.completions.update_query(&self.draft.prompt);
        res.and(ActionResult::redraw())
    }

    pub fn move_cursor(&mut self, make_move: impl FnOnce(&str, usize) -> usize) -> ActionResult {
        let res = self.checkout_from_history();

        self.cursor.prompt_col = make_move(&self.draft.prompt, self.cursor.prompt_col);
        res.and(ActionResult::redraw())
    }

    /// If the current draft prompt is a bare path to a module or type, render
    /// its definition and commit the cell.
    pub fn try_describe(&mut self, project: &lutra_compiler::Project) -> bool {
        let message =
            crate::describe::describe_def(&self.draft.prompt, project).map(|v| v.to_owned());
        if let Some(message) = message {
            self.draft.output = Some(CellOutput::Message(message));
            true
        } else {
            false
        }
    }

    pub fn clear_history(&mut self) {
        self.history.clear();
        self.draft = Cell::new();
        self.cursor.reset();
        self.completions.update_query(&self.draft.prompt);
    }

    /// Move history cursor up (toward older entries).
    pub fn move_history_up(&mut self) {
        let new_idx = match self.cursor.history_cursor {
            Some(cursor) => Some(cursor.saturating_sub(1)),
            None => self.history.len().checked_sub(1),
        };
        if let Some(idx) = new_idx {
            self.cursor.goto_history(idx);
        }
    }

    /// Move history cursor down (toward newer entries or back to draft).
    pub fn move_history_down(&mut self) {
        let Some(cursor) = self.cursor.history_cursor else {
            return;
        };

        if cursor + 1 < self.history.len() {
            self.cursor.goto_history(cursor + 1);
        } else {
            self.cursor.goto_draft(None);
        }
    }

    pub fn last_successful_output(&self) -> Option<CellOutputRef<'_>> {
        self.history
            .last()
            .and_then(|cell| cell.as_ref().as_output())
    }

    pub fn set_execution_result(&mut self, output: Result<Vec<u8>, String>, duration: Duration) {
        let ty = self.draft.program_ty.as_ref();

        let cell_output = match (output, ty) {
            (Ok(_), None) => {
                CellOutput::RunError("internal error: missing program type".to_string())
            }
            (Err(e), _) => CellOutput::RunError(e),
            (Ok(data), Some(ty)) => {
                let layout = table::Table::new(&data, &ty.output, &ty.defs)
                    .compute_layout(table::Config::default());
                CellOutput::RunOutput {
                    data: Rc::new(data),
                    layout: Some(layout),
                    duration,
                }
            }
        };

        let has_layout = matches!(
            &cell_output,
            CellOutput::RunOutput {
                layout: Some(_),
                ..
            }
        );
        let cursor_output = if has_layout {
            OutputCursor::Table(table::Cursor::default())
        } else {
            OutputCursor::Plain { scroll: 0 }
        };

        self.draft.output = Some(cell_output);

        self.cursor.stage = CellStage::Output;
        self.cursor.output = cursor_output;
    }

    pub fn handle(&mut self, action: Action) -> ActionResult {
        match action {
            // Semantic actions from keybindings.
            Action::ClearCell => {
                if self.cursor.history_cursor.is_some() {
                    self.cursor.goto_draft(None);
                    return ActionResult::redraw();
                }
                if !self.draft.is_output_empty() {
                    return self.commit_cell(false);
                }
                if !self.draft.prompt.is_empty() {
                    self.draft.prompt.clear();
                    self.cursor.reset();
                    return ActionResult::redraw();
                }
                if !self.draft.is_bound_input_empty() {
                    self.draft.bound_input = None;
                    return ActionResult::redraw();
                }
                ActionResult::default()
            }
            Action::CycleFocusNext => {
                self.cursor_and_cell(|cursor, cell| {
                    cursor.next_stage(cell);
                });
                ActionResult::redraw()
            }
            Action::CycleFocusPrev => {
                self.cursor_and_cell(|cursor, cell| {
                    cursor.prev_stage(cell);
                });
                ActionResult::redraw()
            }
            Action::HistoryUp => {
                self.move_history_up();
                ActionResult::redraw()
            }
            Action::HistoryDown => {
                self.move_history_down();
                ActionResult::redraw()
            }
            Action::HistorySelect => {
                if !self.visible_cell().is_output_empty() {
                    self.cursor.stage = CellStage::Output;
                } else {
                    self.cursor.stage = CellStage::Program;
                }
                ActionResult::redraw()
            }

            Action::TableMoveUp => {
                self.cursor_and_cell(|cursor, c| {
                    cursor.with_table_mut(c.output_layout(), |cursor, l, page_size| {
                        cursor.row = cursor.row.saturating_sub(1);
                        cursor.scroll_into_view(page_size, l);
                    });
                });
                ActionResult::redraw()
            }
            Action::TableMoveDown => {
                self.cursor_and_cell(|cursor, c| {
                    cursor.with_table_mut(c.output_layout(), |cursor, l, page_size| {
                        cursor.row = (cursor.row + 1).min(l.last_row());
                        cursor.scroll_into_view(page_size, l);
                    });
                });
                ActionResult::redraw()
            }
            Action::TableMoveLeft => {
                self.cursor_and_cell(|cursor, c| {
                    cursor.with_table_mut(c.output_layout(), |cursor, _, _| {
                        cursor.col = cursor.col.saturating_sub(1);
                    });
                });
                ActionResult::redraw()
            }
            Action::TableMoveRight => {
                self.cursor_and_cell(|cursor, c| {
                    cursor.with_table_mut(c.output_layout(), |cursor, layout, _| {
                        cursor.col = (cursor.col + 1).min(layout.last_col());
                    });
                });
                ActionResult::redraw()
            }
            Action::TableMoveHome => {
                self.cursor_and_cell(|cursor, c| {
                    cursor.with_table_mut(c.output_layout(), |cursor, _, _| {
                        cursor.col = 0;
                    });
                });
                ActionResult::redraw()
            }
            Action::TableMoveEnd => {
                self.cursor_and_cell(|cursor, c| {
                    cursor.with_table_mut(c.output_layout(), |cursor, layout, _| {
                        cursor.col = layout.last_col();
                    });
                });
                ActionResult::redraw()
            }
            Action::TableMovePageUp => {
                self.cursor_and_cell(|cursor, c| {
                    cursor.with_table_mut(c.output_layout(), |cursor, l, page_size| {
                        cursor.row = cursor.row.saturating_sub(page_size);
                        cursor.scroll_into_view(page_size, l);
                    });
                });
                ActionResult::redraw()
            }
            Action::TableMovePageDown => {
                self.cursor_and_cell(|cursor, c| {
                    cursor.with_table_mut(c.output_layout(), |cursor, l, page_size| {
                        cursor.row = (cursor.row + page_size).min(l.last_row());
                        cursor.scroll_into_view(page_size, l);
                    });
                });
                ActionResult::redraw()
            }

            Action::CompletionUp => {
                self.completions.select_up();
                ActionResult::redraw()
            }
            Action::CompletionHide => {
                self.completions.clear();
                ActionResult::redraw()
            }
            Action::CompletionDown => {
                self.completions.select_down();
                ActionResult::redraw()
            }
            Action::CompletionApply => {
                if let Some(edit) = self.completions.select() {
                    return self.edit(|_, _| edit);
                }
                ActionResult::redraw()
            }

            Action::EditMoveBackward => self.move_cursor(editor::move_backward),
            Action::EditMoveForward => self.move_cursor(editor::move_forward),
            Action::EditMoveWordBackward => self.move_cursor(editor::move_word_backward),
            Action::EditMoveWordForward => self.move_cursor(editor::move_word_forward),
            Action::EditMoveHome => self.move_cursor(|_, _| 0),
            Action::EditMoveEnd => self.move_cursor(|t, _| t.len()),
            Action::EditDeleteBackward => {
                self.edit(|t, at| Edit::delete(t, at, editor::move_backward))
            }
            Action::EditDeleteWordBackward => {
                self.edit(|t, at| Edit::delete(t, at, editor::move_word_backward))
            }
            Action::EditDeleteForward => {
                self.edit(|t, at| Edit::delete(t, at, editor::move_forward))
            }
            Action::EditDeleteWordForward => {
                self.edit(|t, at| Edit::delete(t, at, editor::move_word_forward))
            }

            // Raw terminal events: only handle text input (Char/Paste).
            // All other keys have already been translated to semantic actions by keybindings.
            // Argument stage is always on the draft cell, never on a history entry.
            Action::Terminal(event) if self.cursor.stage == CellStage::Argument => {
                let Some(argument) = &mut self.draft.argument else {
                    return ActionResult::default();
                };
                match argument.handle(event) {
                    input::InputResult::None => ActionResult::default(),
                    input::InputResult::Redraw => ActionResult::redraw(),
                    input::InputResult::Submit => ActionResult::action(Action::SubmitArgument),
                }
            }
            Action::Terminal(crossterm::event::Event::Key(key)) => match self.cursor.stage {
                CellStage::Browse | CellStage::Program => match key.code {
                    KeyCode::Char(c) if !c.is_control() => {
                        self.edit(|_, at| Edit::insert(at, c.to_string()))
                    }
                    _ => ActionResult::default(),
                },
                _ => ActionResult::default(),
            },
            Action::Terminal(crossterm::event::Event::Paste(text)) => match self.cursor.stage {
                CellStage::Browse | CellStage::Program => self.edit(|_, at| Edit::insert(at, text)),
                _ => ActionResult::default(),
            },

            _ => ActionResult::default(),
        }
    }
}

/// The mutable live editing/inspection state of the current REPL entry.
#[derive(Debug, Clone)]
pub struct Cursor {
    pub stage: CellStage,
    pub output: OutputCursor,
    /// Byte offset of the text cursor within the prompt string.
    pub prompt_col: usize,
    /// Index into the history list (`None` = the live draft).
    pub history_cursor: Option<usize>,
    /// Visible height (in lines) of the output area.
    pub page_size: std::cell::Cell<usize>,
}

impl Cursor {
    pub fn new() -> Self {
        Self {
            stage: CellStage::Program,
            output: OutputCursor::Plain { scroll: 0 },
            prompt_col: 0,
            history_cursor: None,
            page_size: 20.into(),
        }
    }

    /// Navigate to the live draft: clear history selection, enter program stage.
    pub fn goto_draft(&mut self, prompt_col: Option<usize>) {
        self.history_cursor = None;
        self.stage = CellStage::Program;
        if let Some(c) = prompt_col {
            self.prompt_col = c;
        }
    }

    /// Navigate to a history entry and enter browse mode.
    pub fn goto_history(&mut self, idx: usize) {
        self.history_cursor = Some(idx);
        self.stage = CellStage::Browse;
    }

    pub fn reset(&mut self) {
        self.output = OutputCursor::Plain { scroll: 0 };
        self.stage = CellStage::Program;
        self.prompt_col = 0;
        self.history_cursor = None;
    }

    pub fn is_browsing(&self) -> bool {
        matches!(self.stage, CellStage::Browse)
    }
    pub fn on_program(&self) -> bool {
        matches!(self.stage, CellStage::Program)
    }
    pub fn on_argument(&self) -> bool {
        matches!(self.stage, CellStage::Argument)
    }
    pub fn on_output(&self) -> bool {
        matches!(self.stage, CellStage::Output)
    }

    pub fn next_stage(&mut self, cell: &Cell) {
        use CellStage::*;

        for _ in 0..3 {
            self.stage = match self.stage {
                Browse => Program,
                Program => Argument,
                Argument => Output,
                Output => Program,
            };
            if self.stage_visible(cell) {
                return;
            }
        }
        self.stage = Program;
    }

    pub fn prev_stage(&mut self, cell: &Cell) {
        use CellStage::*;

        for _ in 0..3 {
            self.stage = match self.stage {
                Browse => Program,
                Program => Output,
                Argument => Program,
                Output => Argument,
            };
            if self.stage_visible(cell) {
                return;
            }
        }
        self.stage = Program;
    }

    fn stage_visible(&self, cell: &Cell) -> bool {
        match self.stage {
            CellStage::Browse | CellStage::Program => true,
            CellStage::Argument => !cell.is_argument_empty(),
            CellStage::Output => !cell.is_output_empty(),
        }
    }

    fn with_table_mut(
        &mut self,
        layout: Option<&table::Layout>,
        f: impl Fn(&mut table::Cursor, &table::Layout, usize),
    ) {
        let Some(layout) = layout else {
            return;
        };

        if let OutputCursor::Table(cursor) = &mut self.output {
            f(cursor, layout, self.page_size.get())
        }
    }

    /// Convert a cursor into a lutra program that locates the cursor in the `x` value
    pub fn as_program(&self, ty: &lutra_bin::ir::Ty, defs: &[lutra_bin::ir::TyDef]) -> String {
        let mut ty_mat = get_ty_mat(ty, defs);
        let mut program = "x".to_string();

        let OutputCursor::Table(cursor) = &self.output else {
            return program;
        };
        if let lutra_bin::ir::TyKind::Array(item) = &ty_mat.kind {
            program += &format!(" | std::index({}) | std::or_default()", cursor.row);
            ty_mat = get_ty_mat(item, defs);
        }
        if let lutra_bin::ir::TyKind::Tuple(fields) = &ty_mat.kind {
            program += " | x -> x.";

            // TODO: cursor.col indexes nested column space, not just this tuple
            let position = cursor.col;

            let field: String = fields
                .get(position)
                .and_then(|f| f.name.as_ref())
                .cloned()
                .unwrap_or_else(|| position.to_string());
            program += &field;
        }
        program
    }
}

/// How to navigate the output.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OutputCursor {
    /// Line-based scroll offset (for non-table output).
    Plain {
        scroll: usize,
    },
    Table(table::Cursor),
}

impl OutputCursor {
    pub fn as_plain(&self) -> Option<usize> {
        match self {
            OutputCursor::Plain { scroll } => Some(*scroll),
            OutputCursor::Table { .. } => None,
        }
    }
    pub fn as_table(&self) -> Option<&table::Cursor> {
        match self {
            OutputCursor::Plain { .. } => None,
            OutputCursor::Table(cursor) => Some(cursor),
        }
    }
}

/// Resolve a type alias one level deep through `defs`.
fn get_ty_mat<'a>(
    mut ty: &'a lutra_bin::ir::Ty,
    defs: &'a [lutra_bin::ir::TyDef],
) -> &'a lutra_bin::ir::Ty {
    while let lutra_bin::ir::TyKind::Ident(path) = &ty.kind
        && let Some(def) = defs.iter().find(|d| &d.name == path)
    {
        ty = &def.ty
    }
    ty
}
