use std::cell::Cell;
use std::collections::HashSet;

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
    /// Last known viewport height (for ensure_visible).
    viewport_height: Cell<usize>,
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

impl DefKind {
    /// Get the icon character for this definition kind.
    pub fn icon(self, collapsed: bool) -> &'static str {
        match self {
            DefKind::Module => {
                if collapsed {
                    "▸ " // Right arrow - collapsed
                } else {
                    "▾ " // Down arrow - expanded
                }
            }
            DefKind::Function => "ƒ ",
            DefKind::Constant => "c ",
            DefKind::Type => "t ",
        }
    }

    /// Get the icon character for search results (modules always show as collapsed).
    pub fn search_icon(self) -> &'static str {
        match self {
            DefKind::Module => "▸",
            DefKind::Function => "ƒ",
            DefKind::Constant => "c",
            DefKind::Type => "t",
        }
    }

    /// Determine the DefKind from a parser definition.
    pub fn from_def(def: &pr::Def) -> Option<Self> {
        match &def.kind {
            pr::DefKind::Module(_) => Some(DefKind::Module),
            pr::DefKind::Expr(expr_def) => {
                if expr_def.constant {
                    Some(DefKind::Constant)
                } else {
                    Some(DefKind::Function)
                }
            }
            pr::DefKind::Ty(_) => Some(DefKind::Type),
            pr::DefKind::Import(_) | pr::DefKind::Unresolved(_) => None,
        }
    }
}

impl DefinitionsPanel {
    /// Creates a new empty definitions panel.
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            cursor: 0,
            scroll: 0,
            focused: false,
            viewport_height: Cell::new(10), // Default, will be updated on first render
        }
    }

    /// Updates the panel with new type information from compilation.
    pub fn update(&mut self, project: &ProjectState) {
        let CompileResult::Success { project: proj } = &project.compilation else {
            return;
        };

        // Preserve collapsed state before updating
        let old_items = std::mem::take(&mut self.items);
        let collapsed_paths: HashSet<&pr::Path> = (old_items.iter())
            .filter(|item| item.kind == DefKind::Module && item.collapsed)
            .map(|item| &item.path)
            .collect();

        // Create new items from updated project
        self.items = flatten_module(&proj.root_module, &[], 0);

        // Restore collapsed status
        for item in &mut self.items {
            if item.kind == DefKind::Module && collapsed_paths.contains(&item.path) {
                item.collapsed = true;
            }
        }

        // Update visibility based on restored collapsed state
        self.update_visibility();

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

    /// Select a definition by path. Expands parent modules if needed.
    /// Returns true if the path was found and selected.
    pub fn select_path(&mut self, target_path: &ir::Path) -> bool {
        // Convert IR path to PR path for comparison
        let target_pr = pr::Path::new(&target_path.0);

        // Find the item with matching path
        let Some(index) = self.items.iter().position(|item| item.path == target_pr) else {
            return false;
        };

        // Expand all parent modules to make this item visible
        let target_depth = self.items[index].depth;
        for i in 0..=index {
            let item = &mut self.items[i];
            // If this is a module that's an ancestor of our target, expand it
            if item.kind == DefKind::Module && item.depth < target_depth && item.collapsed {
                // Check if target is descendant of this module
                if target_pr.as_steps().starts_with(item.path.as_steps()) {
                    item.collapsed = false;
                }
            }
        }

        // Update visibility after expanding
        self.update_visibility();

        // Set cursor to the target item
        self.cursor = index;
        self.ensure_visible();

        true
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
        // Find the cursor's position in the visible items list
        let visible_items: Vec<usize> = self
            .items
            .iter()
            .enumerate()
            .filter(|(_, item)| item.is_visible)
            .map(|(idx, _)| idx)
            .collect();

        // Find cursor position in visible list
        let Some(cursor_pos) = visible_items.iter().position(|&idx| idx == self.cursor) else {
            return;
        };

        let viewport_height = self.viewport_height.get();

        // Adjust scroll to keep cursor visible
        if cursor_pos < self.scroll {
            // Cursor is above viewport - scroll up
            self.scroll = cursor_pos;
        } else if cursor_pos >= self.scroll + viewport_height {
            // Cursor is below viewport - scroll down
            self.scroll = cursor_pos.saturating_sub(viewport_height - 1);
        }
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
        let block = crate::style::panel_secondary("  Definitions ", self.focused);
        let inner = block.inner(area);
        frame.render_widget(block, area);

        if self.items.is_empty() {
            let empty = Paragraph::new("No definitions");
            frame.render_widget(empty, inner);
            return;
        }

        // Update viewport height for ensure_visible
        self.viewport_height.set(inner.height as usize);

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
        let icon = item.kind.icon(item.collapsed);

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

/// Convert a parser path to an IR path.
pub fn path_to_ir(path: &pr::Path) -> ir::Path {
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

        // Determine kind - skip imports and unresolved
        let Some(kind) = DefKind::from_def(def) else {
            continue;
        };

        match &def.kind {
            pr::DefKind::Module(inner_module) => {
                let children = flatten_module(inner_module, path.as_steps(), depth + 1);

                // Add module itself
                let mut item = DefItem::new(path, kind, depth);
                item.span = def.span;
                items.push(item);

                // Add children
                items.extend(children);
            }
            _ => {
                // Add non-module definition
                let mut item = DefItem::new(path, kind, depth);
                item.span = def.span;
                items.push(item);
            }
        }
    }

    items
}
