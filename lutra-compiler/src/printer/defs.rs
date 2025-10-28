use chumsky::Span;

use crate::pr;
use crate::printer::expr::print_func;
use crate::printer::types::print_ty_func;
use crate::printer::{PrintSource, Printer};

impl PrintSource for pr::Source {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        if self.is_submodule {
            p.push("submodule")?;
            p.new_line();
            p.new_line();
        }

        // defs
        (&self.root, Some(self.span)).print(p)?;

        // trailing new line
        p.new_line();
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        unreachable!()
    }
}

impl PrintSource for (&pr::ModuleDef, Option<crate::Span>) {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        for (i, (name, def)) in self.0.defs.iter().enumerate() {
            if i > 0 {
                p.inject_trivia_inline(def.span.map(|s| s.start()));

                p.new_line();
                p.new_line();
            }

            if let Some(doc_comment) = &def.doc_comment {
                for line in doc_comment.lines() {
                    p.push("## ")?;
                    p.push(line)?;
                    p.new_line();
                }
            }

            p.inject_trivia_leading(def.span.map(|s| s.start), false);

            (name.as_str(), def).print(p)?;
        }

        p.inject_trivia_inline(self.1.map(|s| s.end()));

        p.inject_trivia_trailing(self.1.map(|s| s.end()));

        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        unreachable!()
    }
}

impl PrintSource for (&str, &pr::Def) {
    #[tracing::instrument(name = "d", skip_all)]
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        match &self.1.kind {
            pr::DefKind::Module(module_def) => {
                p.push("module ")?;
                p.push(pr::display_ident(self.0))?;
                p.push(" {")?;
                p.indent();
                p.new_line();

                (module_def, self.1.span).print(p).unwrap();

                p.dedent();
                p.new_line();
                p.push("}")?;
            }
            pr::DefKind::Expr(expr_def) if expr_def.constant => {
                // const
                p.push("const ")?;
                p.push(pr::display_ident(self.0))?;
                if let Some(ty) = &expr_def.ty {
                    p.push(": ")?;
                    ty.print(p)?;
                }
                if let Some(value) = &expr_def.value {
                    p.push(" = ")?;
                    value.print(p)?;
                }
            }
            pr::DefKind::Expr(expr_def) => {
                // func
                if let Some(value) = &expr_def.value {
                    let func = value.kind.as_func().unwrap();

                    print_func(func, Some(self.0), p)?;
                } else {
                    let func = expr_def.ty.as_ref().unwrap().kind.as_func().unwrap();
                    print_ty_func(func, Some(self.0), p)?;
                }
            }
            pr::DefKind::Ty(ty_def) => {
                p.push("type ")?;
                p.push(pr::display_ident(self.0))?;
                p.push(": ")?;
                ty_def.ty.print(p)?;
            }
            pr::DefKind::Import(import_def) => {
                p.push("import ")?;
                p.push(import_def.target.to_string())?;

                if import_def.target.last() != self.0 {
                    p.push(" as ")?;
                    p.push(pr::display_ident(self.0))?;
                }
            }
            pr::DefKind::Unresolved(_) => unreachable!(),
        }

        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        unreachable!()
    }
}
