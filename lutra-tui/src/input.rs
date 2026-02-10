pub mod form;

use lutra_bin::ir;

use crate::panels::InputPane;
use crate::terminal::{Action, Component};

/// Starts a TUI prompt for type `ty` on stdout terminal.
pub fn prompt_for_ty(
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
    initial: Option<lutra_bin::Value>,
) -> Result<lutra_bin::Value, anyhow::Error> {
    let mut app = InputApp {
        pane: InputPane::new(ty, ty_defs),
    };

    if let Some(val) = initial {
        app.pane.set_value(val);
    }

    // Enter terminal
    let mut term = ratatui::init();

    // Listen for terminal events
    let (action_tx, action_rx) = std::sync::mpsc::channel();
    let _event_thread = crate::terminal::spawn_event_reader(action_tx);

    // Run
    let r = crate::terminal::run_action_loop(&mut app, &mut term, action_rx);

    // Restore terminal
    ratatui::restore();

    r?;
    Ok(app.pane.get_value())
}

struct InputApp {
    pane: InputPane,
}

impl Component for InputApp {
    fn handle(&mut self, action: Action) -> crate::terminal::ActionResult {
        if let Action::ExecuteProgram = action {
            crate::terminal::ActionResult::shutdown()
        } else {
            self.pane.handle(action)
        }
    }

    fn render(&self, frame: &mut ratatui::Frame, area: ratatui::prelude::Rect) {
        self.pane.render(frame, area);
    }
}
