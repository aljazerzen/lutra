use crossterm::event::KeyCode;
use lutra_bin::ir;
use lutra_compiler::pr;
use ratatui::prelude::*;
use ratatui::widgets::Paragraph;

use crate::project::{CompileResult, ProjectState};
use crate::style;
use crate::terminal::{Action, ActionResult};

/// Left sidebar displaying project structure as a navigable tree.
pub struct DefinitionsPanel {
    /// Flattened visible items for rendering.
    items: Vec<DefItem>,
    /// Is this panel focused
    pub focused: bool,
    /// Currently focused item index.
    cursor: usize,
    /// Scroll offset for large projects.
    scroll: usize,
}

/// A single item in the definitions tree.
#[derive(Debug, Clone)]
pub struct DefItem {
    /// Full path to the definition.
    pub path: pr::Path,
    /// Kind of definition.
    pub kind: DefKind,
    /// Nesting depth for indentation.
    pub depth: usize,
    /// Source code span of this definition.
    pub span: Option<lutra_compiler::Span>,
    /// Whether this item has an error.
    pub has_error: bool,
    /// For modules: whether children are hidden.
    pub collapsed: bool,
    /// Whether this item should be rendered (not a child of a collapsed module).
    pub is_visible: bool,
}

impl DefItem {
    fn new(path: pr::Path, kind: DefKind, depth: usize) -> Self {
        Self {
            path,
            kind,
            depth,
            span: None,
            has_error: false,
            collapsed: false,
            is_visible: true,
        }
    }
}

/// Kind of definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    Module,
    Function,
    Constant,
    Type,
}

impl DefinitionsPanel {
    /// Creates a new empty definitions panel.
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            cursor: 0,
            scroll: 0,
            focused: false,
        }
    }

    /// Updates the panel with new type information from compilation.
    pub fn update(&mut self, project: &ProjectState) {
        let CompileResult::Success { project: proj } = &project.compilation else {
            return;
        };
        self.items = flatten_module(&proj.root_module, &[], 0);

        // Ensure cursor is valid
        if self.cursor >= self.items.len() && !self.items.is_empty() {
            self.cursor = self.items.len() - 1;
        }
    }

    /// Update error indicators based on diagnostics.
    pub fn update_from_diagnostics(
        &mut self,
        diagnostics: &[lutra_compiler::error::DiagnosticMessage],
    ) {
        for item in &mut self.items {
            let Some(item_span) = item.span else {
                continue;
            };

            item.has_error = diagnostics
                .iter()
                .filter_map(|d| d.span().as_ref())
                .any(|d| d.overlap(&item_span));
        }
    }

    /// Returns the currently selected definition path, if any.
    pub fn selected_path(&self) -> Option<ir::Path> {
        self.items
            .get(self.cursor)
            .map(|item| path_to_ir(&item.path))
    }

    /// Handle a key event. Returns an action if the panel requests app-level behavior.
    pub fn handle(&mut self, action: Action) -> ActionResult {
        // Extract key event from terminal action
        let Some(key) = action.as_key() else {
            return ActionResult::default();
        };

        match key.code {
            KeyCode::Up => {
                if let Some(new_cursor) = self.find_prev_visible(self.cursor) {
                    self.cursor = new_cursor;
                    self.ensure_visible();
                    return ActionResult::redraw();
                }
            }
            KeyCode::Down => {
                if let Some(new_cursor) = self.find_next_visible(self.cursor) {
                    self.cursor = new_cursor;
                    self.ensure_visible();
                    return ActionResult::redraw();
                }
            }
            KeyCode::Enter => {
                if let Some(item) = self.items.get_mut(self.cursor) {
                    match item.kind {
                        DefKind::Module => {
                            // Toggle collapse state
                            item.collapsed = !item.collapsed;
                            self.update_visibility();
                            return ActionResult::redraw();
                        }
                        DefKind::Function | DefKind::Constant => {
                            // Run
                            return ActionResult::action(Action::RunDefinition(path_to_ir(
                                &item.path,
                            )));
                        }
                        DefKind::Type => {
                            // Types can't be run or collapsed
                        }
                    }
                }
            }
            KeyCode::Right => {
                // Expand module
                if let Some(item) = self.items.get_mut(self.cursor)
                    && let DefKind::Module = item.kind
                {
                    // Toggle collapse state
                    item.collapsed = false;
                    self.update_visibility();
                    return ActionResult::redraw();
                }
            }
            KeyCode::Left => {
                // Try to collapse, if this is a un-collapsed module
                if let Some(item) = self.items.get_mut(self.cursor)
                    && item.kind == DefKind::Module
                    && !item.collapsed
                {
                    item.collapsed = true;
                    self.update_visibility();
                    return ActionResult::redraw();
                }

                // Fallback: navigate to parent module
                if let Some(parent_idx) = self.find_parent_module(self.cursor) {
                    self.cursor = parent_idx;
                    self.ensure_visible();
                    return ActionResult::redraw();
                }
            }
            _ => {}
        }
        ActionResult::default()
    }

    /// Ensure the cursor is visible within the scroll window.
    fn ensure_visible(&mut self) {
        // This will be implemented when we know the viewport height
    }

    /// Update is_visible flags based on collapsed state of modules.
    fn update_visibility(&mut self) {
        let mut collapsed_depth: Option<usize> = None;

        for item in &mut self.items {
            // If we're inside a collapsed region
            if let Some(min_depth) = collapsed_depth {
                if item.depth > min_depth {
                    item.is_visible = false;
                    continue;
                } else {
                    // Back to same or shallower depth - exit collapsed region
                    collapsed_depth = None;
                }
            }

            // This item is visible
            item.is_visible = true;

            // If this is a collapsed module, start hiding children
            if item.kind == DefKind::Module && item.collapsed {
                collapsed_depth = Some(item.depth);
            }
        }
    }

    /// Find the next visible item after the given index.
    fn find_next_visible(&self, from: usize) -> Option<usize> {
        self.items[from + 1..]
            .iter()
            .position(|item| item.is_visible)
            .map(|offset| from + 1 + offset)
    }

    /// Find the previous visible item before the given index.
    fn find_prev_visible(&self, from: usize) -> Option<usize> {
        self.items[..from].iter().rposition(|item| item.is_visible)
    }

    /// Find the parent module of the item at the given index.
    fn find_parent_module(&self, from: usize) -> Option<usize> {
        let current_depth = self.items.get(from)?.depth;

        // Scan backwards for first module with smaller depth
        for idx in (0..from).rev() {
            let item = &self.items[idx];
            if item.depth < current_depth && item.kind == DefKind::Module {
                return Some(idx);
            }
        }

        None
    }

    /// Render the panel to the given area.
    pub fn render(&self, frame: &mut Frame, area: Rect) {
        let block = crate::style::panel_secondary(" Definitions ", self.focused);
        let inner = block.inner(area);
        frame.render_widget(block, area);

        if self.items.is_empty() {
            let empty = Paragraph::new("No definitions");
            frame.render_widget(empty, inner);
            return;
        }

        // Filter to visible items only
        let visible_items: Vec<(usize, &DefItem)> = self
            .items
            .iter()
            .enumerate()
            .filter(|(_, item)| item.is_visible)
            .collect();

        let visible_height = inner.height as usize;
        let start = self.scroll.min(visible_items.len().saturating_sub(1));
        let end = (start + visible_height).min(visible_items.len());

        for (i, (original_idx, item)) in visible_items[start..end].iter().enumerate() {
            let y = inner.y + i as u16;
            if y >= inner.y + inner.height {
                break;
            }

            let item_area = Rect {
                x: inner.x,
                y,
                width: inner.width,
                height: 1,
            };

            let is_selected = *original_idx == self.cursor;
            self.render_item(frame, item_area, item, is_selected, self.focused);
        }
    }

    fn render_item(
        &self,
        frame: &mut Frame,
        area: Rect,
        item: &DefItem,
        is_selected: bool,
        panel_focused: bool,
    ) {
        let indent = "  ".repeat(item.depth);
        let icon = match item.kind {
            DefKind::Module => {
                if item.collapsed {
                    "▸ " // Right arrow - collapsed
                } else {
                    "▾ " // Down arrow - expanded
                }
            }
            DefKind::Function => "ƒ ",
            DefKind::Constant => "c ",
            DefKind::Type => "t ",
        };

        let text_style = if is_selected && panel_focused {
            Style::default().bg(Color::White).fg(Color::Black)
        } else {
            Style::default()
        };
        let line_style = if is_selected {
            Style::default().bg(style::COLOR_BG_SECONDARY_ACTIVE)
        } else {
            Style::default()
        };

        let line = Line::from(vec![
            Span::raw(&indent),
            Span::styled(icon, Style::default().fg(Color::DarkGray)),
            Span::styled(item.path.last(), text_style),
        ]);

        frame.render_widget(line.style(line_style), area);

        if item.has_error {
            let dot = Span::raw(" ● ").fg(Color::Red);
            let [_, dot_area] =
                Layout::horizontal([Constraint::Min(0), Constraint::Length(3)]).areas(area);
            frame.render_widget(dot, dot_area);
        }
    }
}

fn path_to_ir(path: &pr::Path) -> ir::Path {
    ir::Path(path.clone().into_iter().collect())
}

/// Flatten a module into a list of DefItems for display.
fn flatten_module(module: &pr::ModuleDef, path: &[String], depth: usize) -> Vec<DefItem> {
    let mut items = Vec::new();

    for (name, def) in module.defs.iter() {
        // Skip std module
        if name == "std" {
            continue;
        }

        let mut path = pr::Path::new(path);
        path.push(name.to_string());

        match &def.kind {
            pr::DefKind::Module(inner_module) => {
                let children = flatten_module(inner_module, path.as_steps(), depth + 1);

                // Add module itself
                let mut item = DefItem::new(path, DefKind::Module, depth);
                item.span = def.span;
                items.push(item);

                // Add children
                items.extend(children);
            }
            pr::DefKind::Expr(expr_def) => {
                // Distinguish function vs constant
                let kind = if expr_def.constant {
                    DefKind::Constant
                } else {
                    DefKind::Function
                };

                let mut item = DefItem::new(path, kind, depth);
                item.span = def.span;
                items.push(item);
            }
            pr::DefKind::Ty(_) => {
                let mut item = DefItem::new(path, DefKind::Type, depth);
                item.span = def.span;
                items.push(item);
            }
            pr::DefKind::Import(_) => {
                // Skip imports - not currently shown in UI
            }
            pr::DefKind::Unresolved(_) => {
                // Skip unresolved definitions - shouldn't exist in resolved modules
            }
        }
    }

    items
}
