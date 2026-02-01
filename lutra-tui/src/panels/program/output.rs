use std::rc::Rc;
use std::time::Duration;

use lutra_bin::rr;
use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Padding, Paragraph};

use crate::terminal::{Action, Component, EventResult};

/// Output stage component - displays execution results
pub struct OutputPane {
    // Shared state
    program_ty: Rc<rr::ProgramType>,

    // Output-specific state (preserved across transitions)
    output: Vec<u8>,
    duration: Duration,
    scroll: usize,
}

impl OutputPane {
    pub fn new(program_ty: Rc<rr::ProgramType>, output: Vec<u8>, duration: Duration) -> Self {
        Self {
            program_ty,
            output,
            duration,
            scroll: 0,
        }
    }
}

impl Component for OutputPane {
    fn render(&self, frame: &mut Frame, area: Rect) {
        let block = Block::default()
            .title(" Output ")
            .borders(Borders::TOP | Borders::LEFT)
            .padding(Padding::left(1))
            .border_style(Style::default().fg(Color::DarkGray));

        let inner = block.inner(area);
        frame.render_widget(block, area);

        let mut y = inner.y;

        // Header with duration
        let header = format!("Completed in {:.2?}", self.duration);
        frame.render_widget(
            Span::styled(header, Style::default().fg(Color::Green)),
            Rect {
                y,
                height: 1,
                ..inner
            },
        );
        y += 2;

        // Output value
        let output_area = Rect {
            x: inner.x,
            y,
            width: inner.width,
            height: inner.height.saturating_sub(y - inner.y),
        };

        let text =
            lutra_bin::print_source(&self.output, &self.program_ty.output, &self.program_ty.defs)
                .unwrap_or_else(|e| format!("Error formatting output: {e}"));
        let paragraph = Paragraph::new(text).scroll((self.scroll as u16, 0));
        frame.render_widget(paragraph, output_area);
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
            Action::Select => {
                // Return to input state
                EventResult::action(Action::ReturnToInput)
            }
            _ => EventResult::default(),
        }
    }
}
