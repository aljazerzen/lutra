use ratatui::prelude::*;
use ratatui::widgets::{Block, Borders, Padding};

pub static COLOR_FG_ACCENT: Color = Color::from_u32(0x006da0ca);
pub static COLOR_BG_ACCENT: Color = Color::from_u32(0x004b657b);
pub static COLOR_BG_PRIMARY: Color = Color::from_u32(0x00222222);
pub static COLOR_BG_SECONDARY: Color = Color::from_u32(0x00333333);
pub static COLOR_BG_SECONDARY_ACTIVE: Color = Color::from_u32(0x003f3f3f);

pub fn panel_primary(title: &str, focused: bool) -> Block<'_> {
    Block::default()
        .title(title)
        .borders(Borders::TOP)
        .border_style(if focused {
            Style::default().fg(COLOR_FG_ACCENT)
        } else {
            Style::default().fg(Color::DarkGray)
        })
        .bg(COLOR_BG_PRIMARY)
        .padding(Padding::left(1))
}

pub fn panel_secondary(title: &str, focused: bool) -> Block<'_> {
    Block::default()
        .title(title)
        .borders(Borders::TOP)
        .border_style(if focused {
            Style::default().fg(COLOR_FG_ACCENT)
        } else {
            Style::default().fg(Color::DarkGray)
        })
        .bg(COLOR_BG_SECONDARY)
}
