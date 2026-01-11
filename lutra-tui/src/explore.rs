use layout::Offset;
use lutra_bin::ir;
use ratatui::prelude::*;

use crate::terminal::{Action, EventResult};

pub fn prompt_for_def(project: &ir::Module) -> anyhow::Result<ir::Path> {
    let mut app = ExploreApp::new(project);

    crate::terminal::within_alternate_screen(|term| crate::terminal::run_app(&mut app, term))??;

    let path = app.root_def.get_path(&app.cursor);
    Ok(ir::Path(path))
}

struct ExploreApp {
    root_def: Decl,
    cursor: Vec<usize>,
}

impl ExploreApp {
    fn new(project: &ir::Module) -> Self {
        let root_def = Decl {
            name: "".into(),
            kind: DeclKind::Module(ModuleDecl::new(project)),
            focus: false,
        };

        let mut app = ExploreApp {
            root_def,
            cursor: vec![],
        };

        app.update_cursor_path_position(|p| p);
        app
    }

    fn update_cursor_path_position(&mut self, update: impl FnOnce(usize) -> usize) {
        let (mut position, found) = self.root_def.take_focus();

        if !found {
            position = 0;
        }

        let updated_position = update(position);
        let res = self.root_def.insert_focus(updated_position);

        let Ok(path) = res else {
            // revert
            self.root_def.insert_focus(position).ok();
            return;
        };
        self.cursor = path;
    }
}

impl crate::terminal::Component for ExploreApp {
    fn render(&self, frame: &mut ratatui::Frame) {
        let DeclKind::Module(ModuleDecl { decls, .. }) = &self.root_def.kind else {
            panic!()
        };

        let mut area = frame.area().offset(Offset { x: 2, y: 0 });
        for decl in decls {
            area.y += 1;
            area = decl.render(frame, area);
        }
    }

    fn update(&mut self, action: Action) -> EventResult {
        let mut res = EventResult::default();
        match action {
            Action::MoveUp => {
                self.update_cursor_path_position(|p| p.saturating_sub(1));
            }
            Action::MoveDown => {
                self.update_cursor_path_position(|p| p.saturating_add(1));
            }
            Action::Select => {
                res.shutdown = true;
            }
            _ => {}
        }
        res
    }
}

struct Decl {
    name: String,
    kind: DeclKind,
    focus: bool,
}

impl Decl {
    fn new(name: &str, decl: &ir::Decl) -> Option<Self> {
        let kind = match decl {
            ir::Decl::Module(module) => DeclKind::Module(ModuleDecl::new(module)),
            ir::Decl::Var(ty) => {
                if ty.kind.is_function() {
                    DeclKind::Function(FunctionDecl::new(ty))
                } else {
                    DeclKind::Value(ValueDecl::new(ty.clone()))
                }
            }
            ir::Decl::Type(ty) => DeclKind::Ty(TypeDecl::new(ty.clone())),
        };
        Some(Decl {
            kind,
            name: name.to_string(),
            focus: false,
        })
    }

    fn render(&self, frame: &mut ratatui::Frame, area: Rect) -> Rect {
        if self.focus {
            let arrow_area = area.offset(Offset {
                x: -(area.left() as i32),
                y: 0,
            });
            frame.render_widget(">".white(), arrow_area)
        }

        match &self.kind {
            DeclKind::Module(d) => d.render(self, frame, area),
            DeclKind::Function(d) => d.render(self, frame, area),
            DeclKind::Value(d) => d.render(self, frame, area),
            DeclKind::Ty(d) => d.render(self, frame, area),
        }
    }

    fn name_widget(&self) -> Span<'_> {
        if self.focus {
            self.name.as_str().black().on_white()
        } else {
            self.name.as_str().white()
        }
    }

    fn take_focus(&mut self) -> (usize, bool) {
        if self.focus {
            self.focus = false;
            return (0, true);
        }

        let passed_defs = match &mut self.kind {
            DeclKind::Function(_) => 1,
            DeclKind::Value(_) => 1,
            DeclKind::Ty(_) => 1,
            DeclKind::Module(module) => {
                let mut position = 0;
                for decl in &mut module.decls {
                    let (p, found) = decl.take_focus();
                    position += p;
                    if found {
                        return (position, true);
                    }
                }
                position
            }
        };
        (passed_defs, false)
    }

    fn insert_focus(&mut self, position: usize) -> Result<Vec<usize>, usize> {
        match &mut self.kind {
            DeclKind::Function(_) | DeclKind::Value(_) | DeclKind::Ty(_) => {
                if position == 0 {
                    self.focus = true;
                    Ok(vec![])
                } else {
                    Err(position - 1)
                }
            }
            DeclKind::Module(module) => {
                let mut position = position;
                for (index, decl) in module.decls.iter_mut().enumerate() {
                    match decl.insert_focus(position) {
                        Ok(mut path) => {
                            path.insert(0, index);
                            return Ok(path);
                        }
                        Err(p) => {
                            position = p;
                        }
                    }
                }
                Err(position)
            }
        }
    }

    fn get_path(&self, steps: &[usize]) -> Vec<String> {
        if steps.is_empty() {
            return vec![];
        }
        let DeclKind::Module(module) = &self.kind else {
            panic!();
        };
        let decl = module.decls.get(steps[0]).unwrap();
        let mut path = decl.get_path(&steps[1..]);
        path.insert(0, decl.name.clone());
        path
    }
}

enum DeclKind {
    Module(ModuleDecl),
    Function(FunctionDecl),
    Value(ValueDecl),
    Ty(TypeDecl),
}

struct ModuleDecl {
    decls: Vec<Decl>,
}
impl ModuleDecl {
    fn new(module: &ir::Module) -> Self {
        let decls = module
            .decls
            .iter()
            .filter(|item| item.name != "std")
            .flat_map(|item| Decl::new(&item.name, &item.decl))
            .collect();
        Self { decls }
    }

    fn render(&self, decl: &Decl, frame: &mut Frame<'_>, mut area: Rect) -> Rect {
        let line = format!("module {} {{", decl.name);
        frame.render_widget(line.white(), area);

        // inner
        area.x += 2;
        area.width -= 2;
        for d in &self.decls {
            area.y += 1;
            area = d.render(frame, area);
        }

        area.x -= 2;
        area.width += 2;

        frame.render_widget("}".white(), area);

        area.y += 1;
        area
    }
}

struct FunctionDecl {}

impl FunctionDecl {
    fn new(_ty: &ir::Ty) -> Self {
        Self {}
    }

    fn render(&self, decl: &Decl, frame: &mut Frame<'_>, area: Rect) -> Rect {
        let line = Line::from(vec!["let ".white(), decl.name_widget(), ": func".white()]);
        frame.render_widget(line, area);

        area.offset(Offset { x: 0, y: 1 })
    }
}

struct ValueDecl {
    ty: ir::Ty,
}
impl ValueDecl {
    fn new(ty: ir::Ty) -> Self {
        Self { ty }
    }

    fn render(&self, decl: &Decl, frame: &mut Frame<'_>, area: Rect) -> Rect {
        let line = Line::from(vec![
            "let ".white(),
            decl.name_widget(),
            format!(": {}", lutra_bin::ir::print_ty(&self.ty)).white(),
        ]);
        frame.render_widget(line, area);

        if decl.focus {
            frame.render_widget(
                decl.name.as_str().black().on_white(),
                area.offset(Offset { x: 4, y: 0 }),
            );
        }

        area.offset(Offset { x: 0, y: 1 })
    }
}

struct TypeDecl {
    ty: ir::Ty,
}
impl TypeDecl {
    fn new(ty: ir::Ty) -> Self {
        Self { ty }
    }

    fn render(&self, decl: &Decl, frame: &mut Frame<'_>, area: Rect) -> Rect {
        let line = Line::from(vec![
            "type ".white(),
            decl.name_widget(),
            format!(" = {}", lutra_bin::ir::print_ty(&self.ty)).white(),
        ]);
        frame.render_widget(line, area);

        area.offset(Offset { x: 0, y: 1 })
    }
}
