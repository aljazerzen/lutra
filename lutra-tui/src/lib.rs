mod input;
mod interactive;
mod keybindings;
mod layout;
mod panels;
mod project;
mod runner;
mod style;
mod terminal;
mod utils;
mod watcher;

pub use input::prompt_for_ty;
pub use interactive::run_interactive;
pub use runner::RunnerConfig;

pub fn show_value(
    value: &[u8],
    ty: &lutra_bin::ir::Ty,
    ty_defs: &[lutra_bin::ir::TyDef],
) -> Result<(), anyhow::Error> {
    use crossterm::event;
    use ratatui::widgets::Paragraph;
    use ratatui::widgets::Wrap;

    let text = lutra_bin::print_source(value, ty, ty_defs).unwrap();

    let mut term = ratatui::init();

    term.draw(|frame| {
        frame.render_widget(
            Paragraph::new(text).wrap(Wrap { trim: false }),
            frame.area(),
        );
    })?;

    // wait for an event
    event::read()?;

    ratatui::restore();
    Ok(())
}
