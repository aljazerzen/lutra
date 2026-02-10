use crossterm::event::KeyCode;
use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Padding, Paragraph};

use crate::terminal::{Action, ActionResult, Component};

/// Error stage component - displays execution errors
pub struct ErrorPane {
    error: String,
    scroll: usize,
}

impl ErrorPane {
    pub fn new(error: String) -> Self {
        Self { error, scroll: 0 }
    }
}

impl Component for ErrorPane {
    fn render(&self, frame: &mut Frame, area: Rect) {
        let block = Block::default()
            .title(" Error ")
            .borders(Borders::TOP | Borders::LEFT)
            .padding(Padding::left(1))
            .border_style(Style::default().fg(Color::Red));

        let inner = block.inner(area);
        frame.render_widget(block, area);

        let paragraph = Paragraph::new(self.error.as_str())
            .style(Style::default().fg(Color::Red))
            .scroll((self.scroll as u16, 0));
        frame.render_widget(paragraph, inner);
    }

    fn handle(&mut self, action: Action) -> ActionResult {
        let Some(key) = action.as_key() else {
            return ActionResult::default();
        };

        match key.code {
            KeyCode::Up => {
                self.scroll = self.scroll.saturating_sub(1);
                ActionResult::redraw()
            }
            KeyCode::Down => {
                self.scroll = self.scroll.saturating_add(1);
                ActionResult::redraw()
            }
            KeyCode::Enter => ActionResult::action(Action::ReturnToInput),
            _ => ActionResult::default(),
        }
    }
}
