use crossterm::event::{KeyCode, KeyEvent};
use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, List, ListItem, ListState, Padding, Paragraph, Wrap};

use crate::terminal::{Action, ActionResult, Component};

use super::RunnerParams;

/// Runner pane component that handles both selection and configuration.
pub struct RunnerPane {
    // Selection state
    list_state: ListState,

    // Configuration state
    config_input: String,
    config_cursor_pos: usize,

    // Overall state
    state: State,
    focused: bool,
    configured: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum State {
    Selecting,
    Configuring,
}

impl RunnerPane {
    pub fn new_with_initial(initial: Option<RunnerParams>) -> Self {
        let mut list_state = ListState::default();
        list_state.select(Some(0));

        let (selected_index, config_input, configured) = if let Some(params) = initial {
            match params {
                RunnerParams::DuckDB(path) => (0, path, true),
                RunnerParams::PostgreSQL(dsn) => (1, dsn, true),
                RunnerParams::Interpreter => (2, String::new(), true),
            }
        } else {
            (0, String::new(), false)
        };

        list_state.select(Some(selected_index));

        Self {
            list_state,
            config_input,
            config_cursor_pos: 0,
            state: State::Selecting,
            focused: false,
            configured,
        }
    }

    fn move_up(&mut self) {
        if let Some(idx) = self.list_state.selected()
            && idx > 0
        {
            self.list_state.select(Some(idx - 1));
        }
    }

    fn move_down(&mut self) {
        let max = RunnerType::all().len() - 1;
        if let Some(idx) = self.list_state.selected()
            && idx < max
        {
            self.list_state.select(Some(idx + 1));
        }
    }

    pub fn get_selected_type(&self) -> RunnerType {
        let idx = self.list_state.selected().unwrap_or(0);
        RunnerType::all()[idx]
    }

    pub fn is_configuring(&self) -> bool {
        self.state == State::Configuring
    }

    pub fn has_selection(&self) -> bool {
        self.configured
    }

    pub fn to_runner_params(&self) -> Option<RunnerParams> {
        if !self.configured {
            return None;
        }

        Some(match self.get_selected_type() {
            RunnerType::Interpreter => RunnerParams::Interpreter,
            RunnerType::PostgreSQL => RunnerParams::PostgreSQL(self.config_input.clone()),
            RunnerType::DuckDB => RunnerParams::DuckDB(self.config_input.clone()),
        })
    }

    pub fn set_focused(&mut self, focused: bool) {
        self.focused = focused;
    }

    fn start_configuration(&mut self) {
        self.state = State::Configuring;

        // Set default input based on runner type
        self.config_input = match self.get_selected_type() {
            RunnerType::DuckDB => ":memory:".to_string(),
            _ => String::new(),
        };
        self.config_cursor_pos = self.config_input.len();
    }

    fn complete_configuration(&mut self) -> ActionResult {
        if !self.config_input.is_empty() {
            self.configured = true;
            self.state = State::Selecting;
            return ActionResult::action(Action::CycleFocus);
        }
        ActionResult::default()
    }

    pub fn cancel_configuration(&mut self) {
        self.state = State::Selecting;
        self.config_input.clear();
        self.config_cursor_pos = 0;
    }

    fn insert_char(&mut self, c: char) {
        self.config_input.insert(self.config_cursor_pos, c);
        self.config_cursor_pos += 1;
    }

    fn delete_char(&mut self) {
        if self.config_cursor_pos > 0 {
            self.config_input.remove(self.config_cursor_pos - 1);
            self.config_cursor_pos -= 1;
        }
    }

    fn move_cursor_left(&mut self) {
        if self.config_cursor_pos > 0 {
            self.config_cursor_pos -= 1;
        }
    }

    fn move_cursor_right(&mut self) {
        if self.config_cursor_pos < self.config_input.len() {
            self.config_cursor_pos += 1;
        }
    }

    fn handle_key_selecting(&mut self, key: KeyEvent) -> ActionResult {
        match key.code {
            KeyCode::Up | KeyCode::Char('k') => {
                self.move_up();
                ActionResult::redraw()
            }
            KeyCode::Down | KeyCode::Char('j') => {
                self.move_down();
                ActionResult::redraw()
            }
            KeyCode::Enter => {
                let runner_type = self.get_selected_type();
                if runner_type.needs_config() {
                    self.start_configuration();
                    ActionResult::redraw()
                } else {
                    self.configured = true;
                    ActionResult::action(Action::CycleFocus)
                }
            }
            _ => ActionResult::default(),
        }
    }

    fn handle_key_configuring(&mut self, key: KeyEvent) -> ActionResult {
        match key.code {
            KeyCode::Char(c) => {
                self.insert_char(c);
                ActionResult::redraw()
            }
            KeyCode::Backspace => {
                self.delete_char();
                ActionResult::redraw()
            }
            KeyCode::Left => {
                self.move_cursor_left();
                ActionResult::redraw()
            }
            KeyCode::Right => {
                self.move_cursor_right();
                ActionResult::redraw()
            }
            KeyCode::Enter => self.complete_configuration(),
            _ => ActionResult::default(),
        }
    }

    fn render_selecting(&self, frame: &mut Frame, area: Rect) {
        // When focused, show full list and description
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(RunnerType::all().len() as u16 + 1),
                Constraint::Min(1),
            ])
            .split(area);

        // Render list
        let items: Vec<ListItem> = RunnerType::all()
            .iter()
            .map(|rt| ListItem::new(rt.name()))
            .collect();

        let mut list_state = self.list_state;
        let list = List::new(items).highlight_style(
            Style::default()
                .bg(crate::style::COLOR_BG_ACCENT)
                .fg(Color::White)
                .bold(),
        );

        frame.render_stateful_widget(list, chunks[0], &mut list_state);

        // Render description
        let selected_type = self.get_selected_type();
        let desc_text = format!("\n{}", selected_type.description());
        let desc = Paragraph::new(desc_text)
            .wrap(Wrap { trim: true })
            .style(Style::default().fg(Color::Gray));

        frame.render_widget(desc, chunks[1]);
    }

    fn render_configuring(&self, frame: &mut Frame, area: Rect) {
        let runner_type = self.get_selected_type();

        // Split into sections
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(1), // Instruction
                Constraint::Length(1), // Input field
                Constraint::Length(1), // Padding
                Constraint::Min(1),    // Help text
            ])
            .split(area);

        // Instruction
        let instruction = match runner_type {
            RunnerType::PostgreSQL => "Enter PostgreSQL connection string:",
            RunnerType::DuckDB => "Enter DuckDB database path:",
            RunnerType::Interpreter => "Configuration not required",
        };

        let instruction_widget = Paragraph::new(instruction)
            .wrap(Wrap { trim: true })
            .style(Style::default().fg(Color::Gray));
        frame.render_widget(instruction_widget, chunks[0]);

        // Input field
        let input_area = chunks[1];
        let input_block = Block::default()
            .borders(Borders::LEFT)
            .padding(Padding::left(1))
            .border_style(Style::default().fg(crate::style::COLOR_FG_ACCENT))
            .bg(crate::style::COLOR_BG_PRIMARY);

        let input_inner = input_block.inner(input_area);
        frame.render_widget(input_block, input_area);

        // Render input text
        let input_text = if self.config_input.is_empty() {
            Text::from("<enter configuration>").style(Style::default().fg(Color::DarkGray).italic())
        } else {
            Text::from(self.config_input.as_str()).style(Style::default().fg(Color::White))
        };
        frame.render_widget(Paragraph::new(input_text), input_inner);

        // Render cursor
        if self.config_cursor_pos <= self.config_input.len() {
            let cursor_x = input_inner.x + self.config_cursor_pos as u16;
            let cursor_y = input_inner.y;

            if cursor_x < input_inner.x + input_inner.width {
                let cursor_char = self
                    .config_input
                    .chars()
                    .nth(self.config_cursor_pos)
                    .unwrap_or(' ')
                    .to_string();

                let cursor_area = Rect {
                    x: cursor_x,
                    y: cursor_y,
                    width: 1,
                    height: 1,
                };

                frame.render_widget(
                    Paragraph::new(cursor_char)
                        .style(Style::default().bg(Color::White).fg(Color::Black)),
                    cursor_area,
                );
            }
        }

        // Help text
        let help = match runner_type {
            RunnerType::PostgreSQL => {
                "Examples:\n\
                 • postgres://localhost/mydb\n\
                 • postgres://user:pass@localhost:5432/dbname\n\
                 • host=localhost port=5432 dbname=mydb user=postgres"
            }
            RunnerType::DuckDB => {
                "Examples:\n\
                 • :memory: (in-memory database)\n\
                 • ./data.duckdb (local file)\n\
                 • /absolute/path/to/db.duckdb"
            }
            RunnerType::Interpreter => "",
        };

        let help_widget = Paragraph::new(help)
            .wrap(Wrap { trim: true })
            .style(Style::default().fg(Color::DarkGray));
        frame.render_widget(help_widget, chunks[3]);
    }
}

impl Component for RunnerPane {
    fn render(&self, frame: &mut Frame, area: Rect) {
        let block = crate::style::panel_primary("Runner ", self.focused).padding(Padding::ZERO);
        let inner_area = block.inner(area);
        frame.render_widget(block, area);

        if !self.focused {
            let text = if self.configured {
                Span::raw(match self.get_selected_type() {
                    RunnerType::Interpreter => "interpreter".to_string(),
                    RunnerType::PostgreSQL => format!("postgres={}", self.config_input),
                    RunnerType::DuckDB => format!("duckdb={}", self.config_input),
                })
            } else {
                Span::raw("<no selection>")
            };
            frame.render_widget(text.style(Style::default().fg(Color::DarkGray)), inner_area);
            return;
        }

        match self.state {
            State::Selecting => self.render_selecting(frame, inner_area),
            State::Configuring => self.render_configuring(frame, inner_area),
        }
    }

    fn handle(&mut self, action: Action) -> ActionResult {
        match action {
            Action::Terminal(crossterm::event::Event::Key(key)) => match self.state {
                State::Selecting => self.handle_key_selecting(key),
                State::Configuring => self.handle_key_configuring(key),
            },
            _ => ActionResult::default(),
        }
    }
}

/// Available runner types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunnerType {
    Interpreter,
    PostgreSQL,
    DuckDB,
}

impl RunnerType {
    fn all() -> &'static [RunnerType] {
        &[
            RunnerType::DuckDB,
            RunnerType::PostgreSQL,
            RunnerType::Interpreter,
        ]
    }

    pub fn name(&self) -> &'static str {
        match self {
            RunnerType::Interpreter => "Interpreter",
            RunnerType::PostgreSQL => "PostgreSQL",
            RunnerType::DuckDB => "DuckDB",
        }
    }

    fn description(&self) -> &'static str {
        match self {
            RunnerType::Interpreter => {
                "Run bytecode locally with no external dependencies. \
                 Best for development and testing."
            }
            RunnerType::PostgreSQL => {
                "Execute SQL on a PostgreSQL database. \
                 Requires a running PostgreSQL instance and connection string."
            }
            RunnerType::DuckDB => {
                "Execute SQL on a local DuckDB database. \
                 Can use in-memory database or persistent file."
            }
        }
    }

    fn needs_config(&self) -> bool {
        matches!(self, RunnerType::PostgreSQL | RunnerType::DuckDB)
    }
}
