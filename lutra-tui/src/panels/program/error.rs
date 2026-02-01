use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Padding, Paragraph};

use crate::terminal::{Action, Component, EventResult};

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

    fn handle(&mut self, action: Action) -> EventResult {
        match action {
            Action::MoveUp => {
                self.scroll = self.scroll.saturating_sub(1);
                EventResult::redraw()
            }
            Action::MoveDown => {
                self.scroll = self.scroll.saturating_add(1);
                EventResult::redraw()
            }
            Action::Select => EventResult::action(Action::ReturnToInput),
            _ => EventResult::default(),
        }
    }
}
