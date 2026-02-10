use lutra_bin::ir;
use lutra_compiler::pr;
use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Padding, Paragraph};

use crate::terminal::{Action, ActionResult, Component};

/// Source code pane showing the function definition.
pub struct SourcePane {
    /// Lines of source code (truncated to MAX_LINES)
    lines: Vec<String>,
    /// Whether source was truncated
    truncated: bool,
    /// Total number of lines in the original source
    total_lines: usize,
}

impl SourcePane {
    const MAX_LINES: usize = 20;

    pub fn new(path: &ir::Path, project: &lutra_compiler::Project) -> Option<Self> {
        // Convert ir::Path to pr::Path
        let pr_path = pr::Path::new(&path.0);

        // 1. Get the definition by path
        let def = project.root_module.get(&pr_path)?;

        // 2. Get the span
        let span = def.span?;

        // 3. Get the source file content
        let (_f_path, source_content) = project.source.get_by_id(span.source_id)?;

        // 4. Extract the function definition slice
        let source_code = span.get_slice(source_content).trim();

        // 5. Truncate
        let all_lines: Vec<String> = source_code.lines().map(|s| s.to_string()).collect();

        let total_lines = all_lines.len();
        let truncated = total_lines > Self::MAX_LINES;

        let lines = all_lines.into_iter().take(Self::MAX_LINES).collect();

        Some(Self {
            lines,
            truncated,
            total_lines,
        })
    }

    /// Calculate the height needed for this source pane.
    /// Includes: lines + truncation message (if any) + borders/padding
    pub fn height(&self) -> u16 {
        let mut height = self.lines.len() as u16;

        // Add 2 lines if truncated (blank line + "... (N more lines)")
        if self.truncated {
            height += 2;
        }

        // Add 1 for borders
        height += 1;

        height
    }
}

impl Component for SourcePane {
    fn handle(&mut self, _action: Action) -> ActionResult {
        // Source pane is read-only, no interaction
        ActionResult::default()
    }

    fn render(&self, frame: &mut Frame, area: Rect) {
        // Build text content
        let mut text = self.lines.join("\n");
        if self.truncated {
            let remaining = self.total_lines - self.lines.len();
            text.push_str(&format!("\n\n... ({} more lines)", remaining));
        }

        // Create block with title
        let block = Block::default()
            .title(" Source ")
            .borders(Borders::TOP | Borders::LEFT)
            .padding(Padding::left(1))
            .border_style(Style::default().fg(Color::DarkGray));

        let paragraph = Paragraph::new(text)
            .block(block)
            .style(Style::default().fg(Color::Gray));

        frame.render_widget(paragraph, area);
    }
}
