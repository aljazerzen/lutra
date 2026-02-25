use std::fs;
use std::path::{Path, PathBuf};

use crossterm::event::{KeyCode, KeyEvent};
use ratatui::prelude::*;
use ratatui::widgets::{Borders, List, ListItem, ListState};

use crate::terminal::{Action, ActionResult, Component};

/// Project selector component with tree view.
pub struct ProjectSelector {
    root: TreeNode,
    flattened: Vec<(PathBuf, String, NodeType, usize)>,
    selected_index: usize,
    list_state: ListState,
    selection: Option<PathBuf>,
    focused: bool,
}

impl ProjectSelector {
    pub fn new(initial_path: Option<PathBuf>) -> anyhow::Result<Self> {
        let start_path = initial_path
            .unwrap_or_else(|| std::env::current_dir().unwrap_or_else(|_| PathBuf::from(".")));

        let mut root = TreeNode::new(start_path, 0)?;
        root.expanded = true;
        root.load_children()?;

        let mut selector = Self {
            root,
            flattened: Vec::new(),
            selected_index: 0,
            list_state: ListState::default(),
            selection: None,
            focused: true,
        };

        selector.update_flattened();
        selector.list_state.select(Some(0));

        Ok(selector)
    }

    fn update_flattened(&mut self) {
        self.flattened.clear();
        self.root.flatten(&mut self.flattened);

        // Clamp selected index
        if self.selected_index >= self.flattened.len() && !self.flattened.is_empty() {
            self.selected_index = self.flattened.len() - 1;
        }

        self.list_state.select(Some(self.selected_index));
    }

    fn move_up(&mut self) {
        if self.selected_index > 0 {
            self.selected_index -= 1;
            self.list_state.select(Some(self.selected_index));
        }
    }

    fn move_down(&mut self) {
        if self.selected_index + 1 < self.flattened.len() {
            self.selected_index += 1;
            self.list_state.select(Some(self.selected_index));
        }
    }

    fn toggle_current(&mut self) -> anyhow::Result<()> {
        if let Some((path, _, _, _)) = self.flattened.get(self.selected_index)
            && let Some(node) = self.root.find_mut(path)
        {
            node.toggle_expand()?;
            self.update_flattened();
        }
        Ok(())
    }

    fn select_current(&mut self) -> ActionResult {
        if let Some((path, _, node_type, _)) = self.flattened.get(self.selected_index) {
            // Handle parent directory navigation
            if *node_type == NodeType::ParentDir {
                return self.navigate_to_parent();
            }

            let node = TreeNode {
                path: path.clone(),
                name: String::new(),
                node_type: *node_type,
                expanded: false,
                depth: 0,
                children: Vec::new(),
            };

            if node.is_valid_project() {
                self.selection = Some(path.clone());
                return ActionResult::action(Action::CycleFocus);
            }
        }
        ActionResult::default()
    }

    fn navigate_to_parent(&mut self) -> ActionResult {
        if let Some(parent) = self.root.path.parent() {
            // Create new root at parent directory
            if let Ok(mut new_root) = TreeNode::new(parent.to_path_buf(), 0) {
                new_root.expanded = true;
                let _ = new_root.load_children();

                self.root = new_root;
                self.selected_index = 0;
                self.update_flattened();

                return ActionResult::redraw();
            }
        }
        ActionResult::default()
    }

    pub fn has_selection(&self) -> bool {
        self.selection.is_some()
    }

    pub fn get_selection(&self) -> Option<PathBuf> {
        self.selection.clone()
    }

    pub fn set_focused(&mut self, focused: bool) {
        self.focused = focused;
    }

    fn handle_key(&mut self, key: KeyEvent) -> ActionResult {
        match key.code {
            KeyCode::Up | KeyCode::Char('k') => {
                self.move_up();
                ActionResult::redraw()
            }
            KeyCode::Down | KeyCode::Char('j') => {
                self.move_down();
                ActionResult::redraw()
            }
            KeyCode::Right | KeyCode::Char('l') | KeyCode::Enter => {
                if let Some((_, _, node_type, _)) = self.flattened.get(self.selected_index) {
                    // Navigate to parent if it's a parent directory entry
                    return match *node_type {
                        NodeType::ParentDir => self.navigate_to_parent(),
                        NodeType::Dir => {
                            let _ = self.toggle_current();
                            ActionResult::redraw()
                        }
                        NodeType::LutraFile | NodeType::LutraDir => self.select_current(),
                        NodeType::File => ActionResult::default(),
                    };
                }
                ActionResult::default()
            }
            KeyCode::Left | KeyCode::Char('h') => {
                if let Some((path, _, _, _)) = self.flattened.get(self.selected_index)
                    && path.is_dir()
                    && let Some(node) = self.root.find_mut(path)
                    && node.expanded
                {
                    let _ = node.toggle_expand();
                    self.update_flattened();
                }
                ActionResult::redraw()
            }
            _ => ActionResult::default(),
        }
    }
}

impl Component for ProjectSelector {
    fn render(&self, frame: &mut Frame, area: Rect) {
        let title = " Project ";

        let block =
            crate::style::panel_primary(title, self.focused).borders(Borders::TOP | Borders::LEFT);

        let _inner = block.inner(area);

        // When unfocused, show only the current selection
        let items: Vec<ListItem> = if !self.focused {
            if let Some(selection) = &self.selection {
                let display = selection.display().to_string();
                vec![ListItem::new(display).style(Style::default().fg(Color::Gray))]
            } else {
                vec![ListItem::new("<no selection>").style(Style::default().fg(Color::DarkGray))]
            }
        } else {
            // When focused, show full tree
            self.flattened
                .iter()
                .map(|(path, name, node_type, depth)| {
                    let indent = "  ".repeat(*depth);
                    let icon = match node_type {
                        NodeType::File | NodeType::LutraFile => "🗎",
                        NodeType::Dir | NodeType::LutraDir => "🗀",
                        NodeType::ParentDir => "↑",
                    };

                    let display = format!("{}{} {}", indent, icon, name);

                    let style = match node_type {
                        NodeType::LutraDir | NodeType::LutraFile => {
                            Style::default().fg(crate::style::COLOR_FG_ACCENT)
                        }
                        NodeType::File | NodeType::Dir | NodeType::ParentDir => {
                            Style::default().fg(Color::White)
                        }
                    };

                    // Highlight selection
                    let style = if self.selection.as_ref() == Some(path) {
                        style.bg(crate::style::COLOR_BG_PRIMARY_ACTIVE)
                    } else {
                        style
                    };

                    ListItem::new(display).style(style)
                })
                .collect()
        };

        let mut list_state = if self.focused {
            self.list_state
        } else {
            ListState::default()
        };

        let list = List::new(items).block(block).highlight_style(
            Style::default()
                .bg(crate::style::COLOR_BG_ACCENT)
                .fg(Color::White)
                .bold(),
        );

        frame.render_stateful_widget(list, area, &mut list_state);
    }

    fn handle(&mut self, action: Action) -> ActionResult {
        match action {
            Action::Terminal(crossterm::event::Event::Key(key)) => self.handle_key(key),
            _ => ActionResult::default(),
        }
    }
}

/// Type of node in the project tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NodeType {
    File,      // File
    Dir,       // Directory
    LutraFile, // .lt file
    LutraDir,  // Directory with module.lt
    ParentDir, // Special node for navigating to parent (..)
}

/// A node in the project tree.
#[derive(Debug, Clone)]
struct TreeNode {
    path: PathBuf,
    name: String,
    node_type: NodeType,
    expanded: bool,
    depth: usize,
    children: Vec<TreeNode>,
}

impl TreeNode {
    fn new(path: PathBuf, depth: usize) -> std::io::Result<Self> {
        let name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("")
            .to_string();

        let is_dir = path.is_dir();
        let has_module = is_dir && path.join("module.lt").exists();

        let node_type = if !is_dir {
            if path.extension().is_some_and(|e| e == "lt") {
                NodeType::LutraFile
            } else {
                NodeType::File
            }
        } else if has_module {
            NodeType::LutraDir
        } else {
            NodeType::Dir
        };

        Ok(Self {
            path,
            name,
            node_type,
            expanded: false,
            depth,
            children: Vec::new(),
        })
    }

    fn load_children(&mut self) -> std::io::Result<()> {
        if !self.path.is_dir() || !self.children.is_empty() {
            return Ok(());
        }

        let mut entries = Vec::new();

        // Add parent directory entry if not at filesystem root
        if self.path.parent().is_some()
            && let Some(parent_path) = self.path.parent()
        {
            let parent_node = TreeNode {
                path: parent_path.to_path_buf(),
                name: "..".to_string(),
                node_type: NodeType::ParentDir,
                expanded: false,
                depth: self.depth + 1,
                children: Vec::new(),
            };
            entries.push(parent_node);
        }

        for entry in fs::read_dir(&self.path)? {
            let entry = entry?;
            let path = entry.path();

            // Filter: only show .lt files and directories
            let is_dir = path.is_dir();
            let is_lt_file = path.extension().and_then(|s| s.to_str()) == Some("lt");

            if (is_dir || is_lt_file)
                && let Ok(node) = TreeNode::new(path, self.depth + 1)
            {
                entries.push(node);
            }
        }

        // Sort: parent dir first, then directories, then files, alphabetically
        entries.sort_by(|a, b| match (a.node_type, b.node_type) {
            (NodeType::ParentDir, NodeType::ParentDir) => std::cmp::Ordering::Equal,
            (NodeType::ParentDir, _) => std::cmp::Ordering::Less,
            (_, NodeType::ParentDir) => std::cmp::Ordering::Greater,
            _ => match (a.path.is_dir(), b.path.is_dir()) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => a.name.to_lowercase().cmp(&b.name.to_lowercase()),
            },
        });

        self.children = entries;
        Ok(())
    }

    fn toggle_expand(&mut self) -> std::io::Result<()> {
        if !self.path.is_dir() {
            return Ok(());
        }

        self.expanded = !self.expanded;
        if self.expanded && self.children.is_empty() {
            self.load_children()?;
        }
        Ok(())
    }

    fn flatten(&self, result: &mut Vec<(PathBuf, String, NodeType, usize)>) {
        result.push((
            self.path.clone(),
            self.name.clone(),
            self.node_type,
            self.depth,
        ));

        if self.expanded {
            for child in &self.children {
                child.flatten(result);
            }
        }
    }

    fn find_mut(&mut self, path: &Path) -> Option<&mut TreeNode> {
        if self.path == path {
            return Some(self);
        }

        if self.expanded {
            for child in &mut self.children {
                if let Some(node) = child.find_mut(path) {
                    return Some(node);
                }
            }
        }

        None
    }

    fn is_valid_project(&self) -> bool {
        match self.node_type {
            NodeType::File => false,
            NodeType::LutraFile => true,
            NodeType::LutraDir => true,
            NodeType::Dir => false,
            NodeType::ParentDir => false,
        }
    }
}
