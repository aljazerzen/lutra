use std::rc::Rc;

use lutra_bin::{ir, rr};

use crate::export;
use crate::shell::Shell;
use crate::terminal::{ActionResult, Line, Style, View};

#[derive(Default)]
pub struct CommandRegistry {
    commands: Vec<Rc<Command>>,
}

pub struct Command {
    name: String,
    aliases: Vec<String>,
    description: String,
    ty: rr::ProgramType,
    default_input: Option<lutra_bin::Value>,
    run: fn(&mut Shell, Vec<u8>) -> ActionResult,
}

impl Command {
    pub fn new_simple(
        name: &str,
        aliases: &[&str],
        description: &str,
        run: fn(&mut Shell, Vec<u8>) -> ActionResult,
    ) -> Rc<Self> {
        Rc::new(Command {
            name: name.to_string(),
            aliases: aliases.iter().map(|a| a.to_string()).collect(),
            description: description.to_string(),
            default_input: None,
            ty: rr::ProgramType {
                input: ir::Ty::new_unit(),
                output: ir::Ty::new_unit(),
                defs: Default::default(),
            },
            run,
        })
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn aliases(&self) -> &[String] {
        &self.aliases
    }

    pub fn description(&self) -> &str {
        &self.description
    }

    pub fn ty(&self) -> &rr::ProgramType {
        &self.ty
    }

    pub fn default_input(&self) -> Option<&lutra_bin::Value> {
        self.default_input.as_ref()
    }

    pub fn run(&self, shell: &mut Shell, input: Vec<u8>) -> ActionResult {
        (self.run)(shell, input)
    }

    fn matches(&self, name: &str) -> bool {
        self.name == name || self.aliases.iter().any(|alias| alias == name)
    }
}

impl CommandRegistry {
    pub fn get(&self) -> &[Rc<Command>] {
        &self.commands
    }

    pub fn register_buildin(&mut self) {
        self.commands.push(Command::new_simple(
            "pull",
            &[],
            "Fetch schema and rewrite @schema module",
            |s, _| s.pull(),
        ));
        self.commands.push(Rc::new(Command {
            name: "export".to_string(),
            aliases: vec![],
            description: "Export the last successful result".to_string(),
            ty: export::program_ty(),
            default_input: Some(export::default_input()),
            run: export::run,
        }));
        self.commands.push(Command::new_simple(
            "help",
            &[],
            "Show help",
            |s, _| s.repl.commit_message(s.commands.view_help()),
        ));
        self.commands.push(Command::new_simple(
            "clear",
            &[],
            "Clear history and release memory",
            |s, _| {
                s.repl.clear_history();
                ActionResult::redraw()
            },
        ));
        self.commands.push(Command::new_simple(
            "quit",
            &["q", "exit"],
            "Quit",
            |_, _| ActionResult::shutdown(),
        ));
    }

    pub fn parse_and_find(&self, input: &str) -> Option<Result<Rc<Command>, String>> {
        let name = input.trim_start().strip_prefix('/')?;

        // parse
        let name = name.trim();
        if name.is_empty() {
            return Some(Err("expected command name after '/'".to_string()));
        }
        if name.chars().any(char::is_whitespace) {
            return Some(Err(
                "shell commands do not take inline arguments".to_string()
            ));
        }
        let name = name.to_lowercase();

        // find
        let command = self
            .commands
            .iter()
            .find(|command| command.matches(&name))
            .cloned()
            .ok_or_else(|| format!("unknown command /{name}; try /help"));
        Some(command)
    }

    fn view_help(&self) -> View<'static> {
        let width = self
            .commands
            .iter()
            .map(|command| command.name.len())
            .max()
            .unwrap_or_default();

        let mut view = View::new();
        view.push_line(Line::styled("Available commands", Style::new().bold()));
        view.push_line(Line::empty());

        for command in &self.commands {
            let aliases = if command.aliases.is_empty() {
                String::new()
            } else {
                format!(
                    " (aliases: {})",
                    command
                        .aliases
                        .iter()
                        .map(|alias| format!("/{alias}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            view.push_line(Line::new(format!(
                "/{:<width$}  {}{}",
                command.name,
                command.description,
                aliases,
                width = width,
            )));
        }
        view
    }
}
