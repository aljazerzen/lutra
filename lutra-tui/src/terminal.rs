use std::io::stdout;

use crossterm::event::{self, KeyCode, KeyEventKind};
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use crossterm::ExecutableCommand;
use ratatui::prelude::*;

pub trait Component {
    fn render(&self, frame: &mut Frame);

    fn update(&mut self, action: Action);
}

#[derive(Debug)]
pub enum Action {
    Write(String),
    Erase,
    MoveUp,
    MoveDown,
    MoveRight,
    MoveLeft,
    Select,
}

#[derive(Default)]
struct EventResult {
    pub redraw: bool,
    pub shutdown: bool,
}

pub(super) fn run_app(
    app: &mut impl Component,
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
            let res = handle_event(app, event);
            need_redraw = need_redraw || res.redraw;
            if res.shutdown {
                return Ok(());
            }
        }
    }
}

fn handle_event(app: &mut impl Component, event: event::Event) -> EventResult {
    let mut res = EventResult::default();
    match event {
        event::Event::Key(key)
            if key.kind == KeyEventKind::Press && key.code == KeyCode::Char('q') =>
        {
            res.shutdown = true;
        }
        event::Event::Resize(_, _) => {
            res.redraw = true;
        }
        event::Event::Key(event) => {
            let action = match event.code {
                KeyCode::Left => Action::MoveLeft,
                KeyCode::Right => Action::MoveRight,
                KeyCode::Down => Action::MoveDown,
                KeyCode::Up => Action::MoveUp,
                KeyCode::Enter => Action::Select,
                KeyCode::Backspace => Action::Erase,
                KeyCode::Char(char) => Action::Write(char.to_string()),
                _ => return res,
            };
            app.update(action);
            res.redraw = true;
        }
        event::Event::FocusGained => {}
        event::Event::FocusLost => {}
        event::Event::Mouse(_) => {}
        event::Event::Paste(text) => {
            app.update(Action::Write(text));
            res.redraw = true;
        }
    }
    res
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
