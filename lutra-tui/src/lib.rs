mod explore;
mod input;
mod terminal;

pub use explore::prompt_for_def;
pub use input::prompt_for_ty;

pub fn show_value(ty: &lutra_bin::ir::Ty, value: &[u8]) -> Result<(), anyhow::Error> {
    use crossterm::event;
    use ratatui::widgets::Paragraph;
    use ratatui::widgets::Wrap;

    let text = lutra_bin::print_source(value, ty, &[]).unwrap();

    crate::terminal::within_alternate_screen(|term| -> std::io::Result<()> {
        term.draw(|frame| {
            frame.render_widget(Paragraph::new(text).wrap(Wrap { trim: true }), frame.area());
        })?;

        // wait for an event
        event::read()?;

        // close
        Ok(())
    })??;

    Ok(())
}
