mod explore;
mod input;
mod terminal;

pub use explore::prompt_for_def;
pub use input::prompt_for_ty;

pub fn show_value(
    value: &[u8],
    ty: &lutra_bin::ir::Ty,
    ty_defs: &[lutra_bin::ir::TyDef],
) -> Result<(), anyhow::Error> {
    use crossterm::event;
    use ratatui::widgets::Paragraph;
    use ratatui::widgets::Wrap;

    let text = lutra_bin::print_source(value, ty, ty_defs).unwrap();

    crate::terminal::within_alternate_screen(|term| -> std::io::Result<()> {
        term.draw(|frame| {
            frame.render_widget(
                Paragraph::new(text).wrap(Wrap { trim: false }),
                frame.area(),
            );
        })?;

        // wait for an event
        event::read()?;

        // close
        Ok(())
    })??;

    Ok(())
}
