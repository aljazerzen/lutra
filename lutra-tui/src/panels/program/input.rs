use lutra_bin::ir;
use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Padding};
use std::collections::HashMap;
use std::rc::Rc;

use crate::input::form::{Form, FormName, FormResult};
use crate::terminal::{Action, Component, EventResult};

/// Input stage component - displays a form for a given type and a submit button.
pub struct InputPane {
    /// Root of the form tree. Usually a button form.
    form: Form,

    /// Current cursor location, represented as a path into the form tree.
    cursor: Vec<usize>,
}

impl InputPane {
    pub fn new(ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Self {
        let name = if ty.is_unit() {
            "(no input needed)"
        } else {
            "input"
        }
        .to_string();

        // Build type definitions HashMap
        let ty_def_map: HashMap<ir::Path, ir::Ty> = ty_defs
            .iter()
            .map(|def| (def.name.clone(), def.ty.clone()))
            .collect();
        let ty_def_map = Rc::new(ty_def_map);

        let mut form = Form::new_with_submit(ty, FormName::from(name), ty_def_map, "Run");
        let cursor = form.insert_cursor(0).unwrap_or_else(|_| vec![]);

        Self { form, cursor }
    }

    pub fn set_focused(&mut self, focused: bool) {
        // we mark the form as not having cursor,
        // but keep this.cursor, so it can be reinserted
        if let Some(form) = self.form.get_mut(&self.cursor) {
            form.cursor = focused;
        }
    }

    pub fn set_value(&mut self, value: lutra_bin::Value) {
        self.form.set_value(value)
    }

    pub fn get_value(&self) -> lutra_bin::Value {
        self.form.get_value()
    }

    fn update_cursor_position(&mut self, update: impl FnOnce(usize) -> usize) {
        let (position, _) = self.form.take_cursor();
        let updated = update(position);

        match self.form.insert_cursor(updated) {
            Ok(cursor) => {
                self.cursor = cursor;
            }
            Err(_) => {
                // out of bounds, revert
                if let Some(f) = self.form.get_mut(&self.cursor) {
                    f.cursor = true;
                }
            }
        }
    }
}

impl Component for InputPane {
    fn render(&self, frame: &mut Frame, area: Rect) {
        let block = Block::default()
            .title(" Input ")
            .borders(Borders::TOP | Borders::LEFT)
            .padding(Padding::left(1))
            .border_style(Style::default().fg(Color::DarkGray));

        // Render input block
        let inner = block.inner(area);
        let remaining = self.form.render(frame, inner);
        let used = inner.height - remaining.height;

        // Adjust the block size to fit the form
        let mut area = area;
        area.height = used + 1;
        frame.render_widget(block, area);
    }

    fn handle(&mut self, action: Action) -> EventResult {
        // Try to handle with form
        let focused = self.form.get_mut(&self.cursor);
        if let Some(focused) = focused {
            match focused.update(&action) {
                FormResult::Redraw => return EventResult::redraw(),
                FormResult::Submit => return EventResult::action(Action::ExecuteProgram),
                FormResult::None => {}
            }
        }

        // Handle navigation
        match action {
            Action::MoveUp => {
                self.update_cursor_position(|p| p.saturating_sub(1));
                return EventResult::redraw();
            }
            Action::MoveDown | Action::Select => {
                self.update_cursor_position(|p| p.saturating_add(1));
                return EventResult::redraw();
            }
            _ => {}
        }
        EventResult::default()
    }
}
