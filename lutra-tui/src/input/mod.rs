pub mod form;

use std::collections::HashMap;
use std::rc::Rc;

use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use lutra_bin::ir;

use crate::renderer::ShellRenderer;
use crate::terminal::{Action, Line, Rect, Style, View};

use self::form::{Form, FormName, FormResult};

/// Starts a TUI prompt for type `ty` on stdout terminal.
pub fn prompt_for_ty(
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
    initial: Option<lutra_bin::Value>,
) -> Result<lutra_bin::Value, anyhow::Error> {
    let mut pane = InputPane::new(ty, ty_defs, "Submit");
    if let Some(initial) = initial {
        pane.set_value(initial);
    }
    let mut renderer = ShellRenderer::new()?;

    loop {
        let area: Rect = crossterm::terminal::size()?.into();
        renderer.render(pane.render(true), area)?;

        match pane.handle(event::read()?) {
            InputResult::None => {}
            InputResult::Redraw => {}
            InputResult::Submit => break,
        }
    }

    renderer.restore()?;
    Ok(pane.form.get_value())
}

#[derive(Clone)]
pub struct InputPane {
    form: Form,
    cursor: Vec<usize>,
    error: Option<String>,
}

pub enum InputResult {
    None,
    Redraw,
    Submit,
}

impl InputPane {
    pub fn new(ty: &ir::Ty, ty_defs: &[ir::TyDef], submit_label: &'static str) -> Self {
        let name = if ty.is_unit() {
            "(no input needed)"
        } else {
            "input"
        }
        .to_string();

        let ty_def_map: HashMap<ir::Path, ir::Ty> = ty_defs
            .iter()
            .map(|def| (def.name.clone(), def.ty.clone()))
            .collect();
        let ty_def_map = Rc::new(ty_def_map);

        let mut form = Form::new_with_submit(ty, FormName::from(name), ty_def_map, submit_label);
        let cursor = form.insert_cursor(0).unwrap_or_else(|_| vec![]);

        Self {
            form,
            cursor,
            error: None,
        }
    }

    pub fn get_value_bin(&self, ty: &ir::Ty, ty_defs: &[ir::TyDef]) -> Vec<u8> {
        self.form.get_value().encode(ty, ty_defs).unwrap()
    }

    pub fn set_value(&mut self, value: lutra_bin::Value) {
        self.form.set_value(value);
        self.error = None;
    }

    #[allow(dead_code)]
    pub fn set_value_bin(
        &mut self,
        value: &[u8],
        ty: &ir::Ty,
        ty_defs: &[ir::TyDef],
    ) -> Result<(), lutra_bin::Error> {
        self.set_value(lutra_bin::Value::decode(value, ty, ty_defs)?);
        Ok(())
    }

    pub fn set_error(&mut self, error: impl Into<String>) {
        self.error = Some(error.into());
    }

    pub fn render<'a>(&'a self, focused: bool) -> View<'a> {
        let mut view = self.form.view(focused);
        if let Some(error) = &self.error {
            if !view.is_empty() {
                view.push_line(Line::empty());
            }
            view.push_line(Line::styled(error.clone(), Style::danger()));
        }
        view
    }

    pub fn handle(&mut self, event: Event) -> InputResult {
        let action = Action::Terminal(event);
        if let Some(focused) = self.form.get_mut(&self.cursor) {
            match focused.handle(&action) {
                FormResult::Redraw => {
                    self.error = None;
                    return InputResult::Redraw;
                }
                FormResult::Submit => {
                    self.error = None;
                    return InputResult::Submit;
                }
                FormResult::None => {}
            }
        }

        let Some(key) = action.as_key() else {
            return InputResult::None;
        };

        match key.code {
            KeyCode::Up => {
                self.update_cursor_position(|p| p.saturating_sub(1));
                self.error = None;
                InputResult::Redraw
            }
            KeyCode::Down => {
                self.update_cursor_position(|p| p.saturating_add(1));
                self.error = None;
                InputResult::Redraw
            }
            KeyCode::Tab if key.modifiers.contains(KeyModifiers::SHIFT) => {
                self.update_cursor_position(|p| p.saturating_sub(1));
                self.error = None;
                InputResult::Redraw
            }
            KeyCode::Tab => {
                self.update_cursor_position(|p| p.saturating_add(1));
                self.error = None;
                InputResult::Redraw
            }
            _ => InputResult::None,
        }
    }

    fn update_cursor_position(&mut self, update: impl FnOnce(usize) -> usize) {
        let (position, _) = self.form.take_cursor();
        let updated = update(position);

        match self.form.insert_cursor(updated) {
            Ok(cursor) => {
                self.cursor = cursor;
            }
            Err(_) => {
                if let Some(f) = self.form.get_mut(&self.cursor) {
                    f.cursor = true;
                }
            }
        }
    }
}
