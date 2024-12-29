use std::io::stdout;

use crossterm::event::{self, Event};
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use crossterm::ExecutableCommand;
use ratatui::prelude::*;

pub trait App {
    fn render(&self, frame: &mut Frame);

    fn handle_event(&mut self, event: Event) -> EventResult;
}

#[derive(Default)]
pub struct EventResult {
    pub redraw: bool,
    pub shutdown: bool,
}

pub(super) fn run_app(
    app: &mut impl App,
    terminal: &mut Terminal<impl ratatui::backend::Backend>,
) -> Result<(), anyhow::Error> {
    let mut need_redraw = true;
    loop {
        if need_redraw {
            terminal.draw(|frame| app.render(frame))?;
            need_redraw = false;
        }

        while event::poll(std::time::Duration::from_millis(16))? {
            let event = event::read()?;
            let res = app.handle_event(event);
            need_redraw = need_redraw || res.redraw;
            if res.shutdown {
                return Ok(());
            }
        }
    }
}

pub(super) fn within_alternate_screen<O>(
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
