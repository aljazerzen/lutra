mod app;
mod form;

use std::io::stdout;

use app::App;

use crossterm::event;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use crossterm::ExecutableCommand;
use lutra_parser::pr;
use ratatui::prelude::*;

/// Starts a TUI prompt for type `ty` on stdout terminal.
pub fn prompt_for_ty(ty: &pr::Ty) -> Result<lutra_bin::Value, anyhow::Error> {
    let mut app = App::new(ty);

    within_alternate_screen(|term| app.run(term))??;

    Ok(app.get_value())
}

fn within_alternate_screen<O>(
    task: impl FnOnce(&mut Terminal<CrosstermBackend<std::io::Stdout>>) -> O,
) -> std::io::Result<O> {
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;

    let mut terminal = Terminal::new(CrosstermBackend::new(stdout()))?;

    let res = task(&mut terminal);

    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;
    Ok(res)
}

impl App {
    fn run(
        &mut self,
        terminal: &mut Terminal<impl ratatui::backend::Backend>,
    ) -> Result<(), anyhow::Error> {
        let mut need_redraw = true;
        loop {
            if need_redraw {
                terminal.draw(|frame| self.render(frame))?;
                need_redraw = false;
            }

            while event::poll(std::time::Duration::from_millis(16))? {
                let event = event::read()?;
                let res = self.handle_event(event);
                need_redraw = need_redraw || res.redraw;
                if res.shutdown {
                    return Ok(());
                }
            }
        }
    }
}
