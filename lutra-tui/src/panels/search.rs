use crossterm::event::KeyCode;
use lutra_bin::ir;
use lutra_compiler::pr;
use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Clear, List, ListItem, Padding, Paragraph};

use super::definitions::{DefKind, path_to_ir};
use crate::project::{CompileResult, ProjectState};
use crate::style;
use crate::terminal::{Action, ActionResult};

/// Search modal for quickly finding definitions across the project.
pub struct SearchModal {
    /// Search query
    query: String,

    /// All definitions (flat list)
    all_items: Vec<SearchItem>,

    /// Filtered items based on query
    filtered_items: Vec<usize>, // Indices into all_items

    /// Currently selected index in filtered list
    cursor: usize,

    /// Whether the modal is currently active/visible
    pub active: bool,
}

/// A searchable definition item
#[derive(Debug, Clone)]
pub struct SearchItem {
    /// Full path to the definition
    pub path: pr::Path,

    /// Full path as string (cached for search performance)
    path_str: String,

    /// Kind of definition
    pub kind: DefKind,
}

impl SearchModal {
    pub fn new() -> Self {
        Self {
            query: String::new(),
            all_items: Vec::new(),
            filtered_items: Vec::new(),
            cursor: 0,
            active: false,
        }
    }

    /// Update the search modal with new project data
    pub fn update(&mut self, project: &ProjectState) {
        let CompileResult::Success { project: proj } = &project.compilation else {
            self.all_items.clear();
            self.filtered_items.clear();
            return;
        };

        self.all_items = flatten_definitions(&proj.root_module, &[]);
        self.update_filtered();
    }

    /// Update filtered items based on current query
    fn update_filtered(&mut self) {
        if self.query.is_empty() {
            // Show all items
            self.filtered_items = (0..self.all_items.len()).collect();
        } else {
            let query = self.query.to_lowercase();
            self.filtered_items = self
                .all_items
                .iter()
                .enumerate()
                .filter(|(_, item)| item.path_str.contains(&query))
                .map(|(idx, _)| idx)
                .collect();
        }

        // Reset cursor if out of bounds
        if self.cursor >= self.filtered_items.len() {
            self.cursor = self.filtered_items.len().saturating_sub(1);
        }
    }

    /// Clear the search query and reset state
    pub fn clear(&mut self) {
        self.query.clear();
        self.cursor = 0;
        self.update_filtered();
    }

    /// Get the currently selected definition path
    pub fn selected_path(&self) -> Option<ir::Path> {
        self.filtered_items
            .get(self.cursor)
            .and_then(|&idx| self.all_items.get(idx))
            .map(|item| path_to_ir(&item.path))
    }

    /// Handle key event
    pub fn handle(&mut self, action: Action) -> ActionResult {
        let Some(key) = action.as_key() else {
            return ActionResult::default();
        };

        match key.code {
            KeyCode::Esc => {
                // Close modal
                ActionResult::action(Action::DefSearchClose)
            }
            KeyCode::Enter => {
                // Select current item
                if let Some(path) = self.selected_path() {
                    return ActionResult::action(Action::RunDefinition(path));
                }
                ActionResult::default()
            }
            KeyCode::Up => {
                if self.cursor > 0 {
                    self.cursor -= 1;
                }
                ActionResult::redraw()
            }
            KeyCode::Down => {
                if self.cursor + 1 < self.filtered_items.len() {
                    self.cursor += 1;
                }
                ActionResult::redraw()
            }
            KeyCode::Backspace => {
                self.query.pop();
                self.update_filtered();
                ActionResult::redraw()
            }
            KeyCode::Char(c) => {
                self.query.push(c);
                self.update_filtered();
                ActionResult::redraw()
            }
            _ => ActionResult::default(),
        }
    }

    /// Render the search modal
    pub fn render(&self, frame: &mut Frame, area: Rect) {
        // Use ratatui layout functions to center the modal
        let modal_width = area.width.min(80);
        let modal_height = area.height.min(30);

        // Center horizontally
        let horizontal = Layout::horizontal([
            Constraint::Fill(1),
            Constraint::Length(modal_width),
            Constraint::Fill(1),
        ])
        .split(area);

        // Center vertically
        let vertical = Layout::vertical([
            Constraint::Fill(1),
            Constraint::Length(modal_height),
            Constraint::Fill(1),
        ])
        .split(horizontal[1]);

        let modal_area = vertical[1];

        // Clear the modal area to hide content underneath
        frame.render_widget(Clear, modal_area);

        // Render modal block with background
        let block = Block::default()
            .title(" Search ")
            .borders(Borders::TOP | Borders::LEFT)
            .border_style(Style::default().fg(style::COLOR_FG_ACCENT))
            .padding(Padding::horizontal(1))
            .bg(style::COLOR_BG_PRIMARY);

        let inner = block.inner(modal_area);
        frame.render_widget(block, modal_area);

        // Split into search box and results
        let chunks = Layout::vertical([
            Constraint::Length(2), // Search input
            Constraint::Min(0),    // Results list
        ])
        .split(inner);

        // Render search input
        frame.render_widget(Paragraph::new(self.query.as_str()), chunks[0]);

        // Set cursor
        let cursor_x = chunks[0].x + self.query.len() as u16;
        frame.set_cursor_position(Position::new(cursor_x, chunks[0].y));

        // Render results
        if self.filtered_items.is_empty() {
            let empty = Paragraph::new("No matches");
            frame.render_widget(empty, chunks[1]);
            return;
        }

        let visible_height = chunks[1].height as usize;

        // Calculate scroll position to keep cursor visible
        let start = if self.cursor < visible_height {
            0
        } else {
            self.cursor - visible_height + 1
        };
        let end = (start + visible_height).min(self.filtered_items.len());

        let items: Vec<ListItem> = self.filtered_items[start..end]
            .iter()
            .enumerate()
            .map(|(i, &item_idx)| {
                let item = &self.all_items[item_idx];
                let is_selected = start + i == self.cursor;

                let icon = item.kind.search_icon();

                let text = format!("{} {}", icon, item.path_str);
                let style = if is_selected {
                    Style::default().bg(Color::White).fg(Color::Black)
                } else {
                    Style::default()
                };

                ListItem::new(text).style(style)
            })
            .collect();

        let list = List::new(items);
        frame.render_widget(list, chunks[1]);
    }
}

/// Flatten all definitions into a searchable list (no hierarchy)
fn flatten_definitions(module: &pr::ModuleDef, path: &[String]) -> Vec<SearchItem> {
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

        let path_segments: Vec<String> = path.clone().into_iter().collect();
        let path_str = path_segments.join("::").to_lowercase();

        // Add the item itself
        items.push(SearchItem {
            path: path.clone(),
            path_str,
            kind,
        });

        // If it's a module, recursively add children
        if let pr::DefKind::Module(inner_module) = &def.kind {
            items.extend(flatten_definitions(inner_module, path.as_steps()));
        }
    }

    items
}
