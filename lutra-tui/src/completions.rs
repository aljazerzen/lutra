use std::fmt::Write;
use std::rc::Rc;

use lutra_compiler::pr;

use crate::commands::{self, Command};
use crate::editor::Edit;
use crate::project::{CompileResult, ProjectState};
use crate::terminal::{Line, Style, View};

pub struct Completions {
    commands: Vec<Item>,
    definitions: Vec<Item>,
    filtered: Vec<Item>,
    query: Option<Query>,
    selected: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Query {
    search: String,
    at: usize,
    delete: usize,
}

/// A searchable definition item.
#[derive(Debug, Clone)]
pub struct Item {
    /// Full path as lowercased string (cached for search performance).
    searchable: String,

    /// Kind of definition.
    icon: Option<DefIcon>,

    /// Displayed & inserted into repl
    label: String,

    description: Option<String>,
}

impl Completions {
    pub fn new() -> Self {
        Self {
            definitions: Vec::new(),
            commands: Vec::new(),
            filtered: Vec::new(),
            query: None,
            selected: 0,
        }
    }

    pub fn update_commands(&mut self, commands: &[Rc<commands::Command>]) {
        self.commands = commands
            .iter()
            .map(|command| Item::from(command.as_ref()))
            .collect();
        self.filtered = self.filter();
        self.selected = 0;
    }

    /// Update the completion index with new project data.
    pub fn update_project(&mut self, project: &ProjectState) {
        let CompileResult::Success { project: proj } = &project.compilation else {
            return;
        };

        self.definitions = flatten_definitions(&proj.root_module, &[]);
        self.filtered = self.filter();
        self.selected = 0;
    }

    /// Sync the completion query from the current program text.
    ///
    /// Resets selection and narrowed state when the query changes.
    pub fn update_query(&mut self, program: &str) {
        let query = extract_query(program);

        if query == self.query {
            return;
        }
        self.query = query;
        self.filtered = self.filter();
        self.selected = 0;
    }

    /// Build completion suggestions for the given query.
    fn filter(&self) -> Vec<Item> {
        const MAX_ITEMS: usize = 5;

        let Some(query) = &self.query else {
            return Vec::new();
        };

        let haystack = if query.search.starts_with('/') {
            &self.commands
        } else {
            &self.definitions
        };

        let res: Vec<_> = haystack
            .iter()
            .filter(|item| item.searchable.contains(&query.search))
            .take(MAX_ITEMS)
            .cloned()
            .collect();

        // exact match?
        if res
            .iter()
            .any(|item| item.label == query.search && item.icon != Some(DefIcon::Module))
        {
            return Vec::new();
        }

        res
    }

    /// Return suggestion lines ready for rendering.
    pub fn view(&self) -> View<'static> {
        let selected = self.selected.min(self.filtered.len().saturating_sub(1));

        let mut view = View::new();
        for (idx, item) in self.filtered.iter().enumerate() {
            let style = if idx == selected {
                Style::cursor()
            } else {
                Style::new()
            };
            view.push_line(Line::styled(item.to_string(), style));
        }
        view
    }

    pub fn is_empty(&self) -> bool {
        self.filtered.is_empty()
    }

    pub fn clear(&mut self) {
        self.filtered.clear();
        self.selected = 0;
    }

    /// Move selection up (wraps around).
    pub fn select_up(&mut self) {
        if self.filtered.is_empty() {
            return;
        }

        let last = self.filtered.len().saturating_sub(1);
        self.selected = self.selected.min(last);
        self.selected = if self.selected == 0 {
            last
        } else {
            self.selected - 1
        };
    }

    /// Move selection down (wraps around).
    pub fn select_down(&mut self) {
        if self.filtered.is_empty() {
            return;
        }

        let last = self.filtered.len().saturating_sub(1);
        self.selected = self.selected.min(last);
        self.selected = (self.selected + 1) % self.filtered.len();
    }

    /// Return the replacement for the currently selected suggestion.
    pub fn select(&mut self) -> Option<Edit> {
        let query = self.query.as_ref()?;

        let selected = self.selected.min(self.filtered.len().saturating_sub(1));
        let item = self.filtered.get(selected)?;
        let edit = Edit {
            at: query.at,
            move_cursor: isize::MAX,
            delete: query.delete,
            insert: item.label.clone(),
        };
        self.selected = 0;
        self.filtered.clear();
        Some(edit)
    }
}

impl From<&Command> for Item {
    fn from(value: &Command) -> Self {
        Item {
            searchable: format!(
                "/{} {} {}",
                value.name().to_lowercase(),
                value
                    .aliases()
                    .iter()
                    .map(|a| format!("/{a}"))
                    .collect::<Vec<_>>()
                    .join(" "),
                value.description().to_lowercase()
            ),
            icon: None,
            label: format!("/{}", value.name()),
            description: Some(value.description().to_string()),
        }
    }
}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(icon) = self.icon {
            icon.fmt(f)?;
            f.write_char(' ')?;
        }
        f.write_str(&self.label)?;
        if let Some(desc) = &self.description {
            f.write_str(" - ")?;
            f.write_str(desc)?;
        }
        Ok(())
    }
}

/// Kind of definition shown in completion suggestions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefIcon {
    Module,
    Function,
    Constant,
    Type,
}

impl DefIcon {
    pub fn char(self) -> char {
        match self {
            DefIcon::Module => 'm',
            DefIcon::Function => 'ƒ',
            DefIcon::Constant => 'c',
            DefIcon::Type => 't',
        }
    }

    pub fn style(self) -> Style {
        match self {
            DefIcon::Module => Style::danger(),
            DefIcon::Function => Style::success(),
            DefIcon::Constant => Style::new(),
            DefIcon::Type => Style::warning(),
        }
    }

    pub fn from_def(def: &pr::Def) -> Option<Self> {
        match &def.kind {
            pr::DefKind::Module(_) => Some(DefIcon::Module),
            pr::DefKind::Expr(expr_def) => {
                if expr_def.constant {
                    Some(DefIcon::Constant)
                } else {
                    Some(DefIcon::Function)
                }
            }
            pr::DefKind::Ty(_) => Some(DefIcon::Type),
            pr::DefKind::Import(_) | pr::DefKind::Unresolved(_) => None,
        }
    }
}

impl std::fmt::Display for DefIcon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.char())
    }
}

/// Flatten all definitions into a searchable list (no hierarchy).
fn flatten_definitions(module: &pr::ModuleDef, path: &[String]) -> Vec<Item> {
    let mut items = Vec::new();

    for (name, def) in module.defs.iter() {
        let mut path = pr::Path::new(path);
        path.push(name.to_string());

        // Determine kind - skip imports and unresolved.
        let Some(icon) = DefIcon::from_def(def) else {
            continue;
        };

        let path_segments: Vec<String> = path.clone().into_iter().collect();
        let display_path = path_segments.join("::");

        items.push(Item {
            searchable: display_path.to_lowercase(),
            icon: Some(icon),
            label: display_path,
            description: None, // TODO doc comment?
        });

        // If it's a module, recursively add children.
        if let pr::DefKind::Module(inner_module) = &def.kind {
            items.extend(flatten_definitions(inner_module, path.as_steps()));
        }
    }

    items
}

fn extract_query(prompt: &str) -> Option<Query> {
    let trim_start = prompt.trim_start();
    if trim_start.starts_with('/') {
        if trim_start.chars().any(char::is_whitespace) {
            return None;
        }
        return Some(Query {
            search: trim_start.to_lowercase(),
            at: 0,
            delete: prompt.len(),
        });
    }

    let mut start = prompt.len();
    for (idx, ch) in prompt.char_indices().rev() {
        if ch.is_alphanumeric() || ch == '_' || ch == ':' {
            start = idx;
        } else {
            break;
        }
    }

    let query = &prompt[start..];
    if query.is_empty() {
        None
    } else {
        Some(Query {
            search: query.to_lowercase(),
            at: start,
            delete: query.len(),
        })
    }
}
