use chumsky::Span;
use lutra_bin::ident;

use crate::pr::{self, ImportDef};
use crate::printer::common::{Between, Separated};
use crate::printer::expr::{print_func, print_func_signature};
use crate::printer::{CONFIG_NO_WRAP, PrintSource, Printer};

/// Produce a compact, single-line signature string for `def` named `name`,
/// suitable for display in an LSP hover popup.
///
/// Returns `None` for [`pr::DefKind::Import`] and
/// [`pr::DefKind::Unresolved`] definitions, which have no meaningful
/// signature to display.
pub fn format_def_signature(name: &str, def: &pr::Def) -> Option<String> {
    let mut p = Printer::new(&CONFIG_NO_WRAP, None);

    match &def.kind {
        pr::DefKind::Expr(expr_def) => {
            if let Some(func) = expr_def.value.kind.as_func() {
                // function: "func name(params): ReturnType"
                // Use the AST Func node to preserve parameter labels.
                print_func_signature(func, Some(name), &mut p)?;

                // When the return type is not explicitly annotated in the
                // source, supplement it from the inferred type.
                if func.return_ty.is_none()
                    && let Some(inferred_ret) = expr_def
                        .value
                        .ty
                        .as_ref()
                        .and_then(|ty| ty.kind.as_func())
                        .and_then(|tf| tf.body.as_deref())
                    {
                        p.push(": ")?;
                        inferred_ret.print(&mut p)?;
                    }
            } else {
                // value: "let name: Type"  /  "const name: Type"
                let ty = expr_def.value.ty.as_ref()?;
                let keyword = if expr_def.constant { "const " } else { "let " };
                p.push(keyword)?;
                p.push(ident::display(name))?;
                p.push(": ")?;
                ty.print(&mut p)?;
            }
        }
        pr::DefKind::Ty(ty_def) => {
            p.push("type ")?;
            p.push(ident::display(name))?;
            if ty_def.is_framed {
                p.push("(")?;
                (&ty_def.framed_label, &ty_def.ty).print(&mut p)?;
                p.push(")")?;
            } else {
                p.push(": ")?;
                ty_def.ty.print(&mut p)?;
            }
        }
        pr::DefKind::Module(_) => {
            p.push("module ")?;
            p.push(ident::display(name))?;
        }
        pr::DefKind::Import(_) | pr::DefKind::Unresolved(_) => return None,
    }

    assert!(p.edits.is_empty());
    Some(p.buffer)
}

impl PrintSource for pr::Source {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        if self.is_submodule {
            p.push("submodule")?;
            p.new_line();
            p.new_line();
        }

        // defs
        (&self.root, Some(self.span)).print(p).unwrap();

        // trailing new line
        p.new_line();
        p.mark_printed(&Some(self.span));
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        unreachable!()
    }
}

impl PrintSource for (&pr::ModuleDef, Option<crate::Span>) {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        // self-annotations
        for ann in &self.0.annotations {
            p.push("@!")?;
            ann.expr.print(p)?;
            p.new_line();
        }
        if !self.0.annotations.is_empty() {
            p.new_line();
        }

        // defs
        let mut last: Option<&pr::Def> = None;
        for (i, (name, def)) in self.0.defs.iter().enumerate() {
            if i > 0 {
                // inject trailing comments of the prev def
                p.inject_trivia_prev_inline(def.span.map(|s| s.start()));

                // inject new-lines
                let condensed = matches!(def.kind, pr::DefKind::Import(_))
                    && last.is_some_and(|l| matches!(l.kind, pr::DefKind::Import(_)));

                p.new_line();
                if !condensed {
                    p.new_line();
                }
            }

            // consume all new-lines before the def
            if let Some(span) = &def.span {
                while p.take_trivia_new_line(span.start).is_some() {}
            }

            // print the def
            let named_def: NamedDef = (name.as_str(), def);
            print_or_skip(&named_def, p);

            last = Some(def);
        }

        p.inject_trivia_prev_inline(self.1.map(|s| s.end()));
        p.inject_trivia_trailing(self.1.map(|s| s.end()));
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        unreachable!()
    }
}

impl PrintSource for pr::DocComment {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        p.inject_trivia_leading(Some(self.span.start));
        for line in self.content.lines() {
            p.push_unchecked("## ");
            p.push_unchecked(line);
            p.new_line();
        }
        while p.take_trivia_new_line(self.span.end()).is_some() {}
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        unreachable!()
    }
}

fn print_or_skip<T: PrintSource>(t: &T, p: &mut Printer) {
    let span = t.span().unwrap();
    p.mark_printed_up_to(span.start);

    let mut normal_print = p.fork();
    if t.print(&mut normal_print).is_some() {
        p.merge(normal_print);
        return;
    }

    // we failed to print, so we save the preceding code and advance the
    // buffer_span, so it skips the node we cannot print

    p.finish_edit();
    p.buffer_span.start = span.end();
    p.buffer_span.len = 0;
    while p.take_trivia(p.buffer_span.start).is_some() {}
}

type NamedDef<'a> = (&'a str, &'a pr::Def);

impl PrintSource for NamedDef<'_> {
    #[tracing::instrument(name = "d", skip_all)]
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        if let Some(doc_comment) = &self.1.doc_comment {
            doc_comment.print(p)?;
        }

        if let Some(a) = self.1.annotations.first() {
            p.inject_trivia_leading(a.expr.span.map(|s| s.start));
        }

        for ann in &self.1.annotations {
            p.push("@")?;
            ann.expr.print(p)?;
            p.new_line();
        }

        p.inject_trivia_leading(self.1.span_name.map(|s| s.start));

        match &self.1.kind {
            pr::DefKind::Module(module_def) => {
                p.push("module ")?;
                p.push(ident::display(self.0))?;
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
                p.push(ident::display(self.0))?;
                if let Some(ty) = &expr_def.ty {
                    p.push(": ")?;
                    ty.print(p)?;
                }

                p.push(" = ")?;
                expr_def.value.print(p)?;
            }
            pr::DefKind::Expr(expr_def) => {
                // func
                let func = expr_def.value.kind.as_func().unwrap();
                print_func(func, Some(self.0), p)?;
            }
            pr::DefKind::Ty(ty_def) => {
                p.push("type ")?;
                p.push(ident::display(self.0))?;

                if ty_def.is_framed {
                    Between {
                        node: &(&ty_def.framed_label, &ty_def.ty),
                        prefix: "(",
                        suffix: ")",
                        span: self.1.span,
                    }
                    .print(p)?;
                } else {
                    p.push(": ")?;
                    ty_def.ty.print(p)?;
                }
            }
            pr::DefKind::Import(import_def) => {
                p.push("import ")?;
                import_def.print(p)?;
            }
            pr::DefKind::Unresolved(_) => unreachable!(),
        }

        p.mark_printed(&self.1.span);
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        self.1.span
    }
}

impl PrintSource for pr::ImportDef {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        match &self.kind {
            pr::ImportKind::Single(path, alias) => {
                p.push(path.to_string())?;

                if let Some(alias) = alias {
                    p.push(" as ")?;
                    p.push(ident::display(alias))?;
                }
                Some(())
            }
            pr::ImportKind::Many(path, parts) => {
                p.push(path.to_string())?;
                p.push("::")?;

                let mut parts: Vec<&ImportDef> = parts.iter().collect();
                parts.sort_by(|a, b| a.path().cmp(b.path()));

                Between {
                    prefix: "(",
                    node: &Separated {
                        nodes: &parts,
                        sep_inline: ", ",
                        sep_line_end: ",",
                    },
                    suffix: ")",
                    span: None,
                }
                .print(p)
            }
        }
    }

    fn span(&self) -> Option<crate::Span> {
        Some(self.span)
    }
}

impl PrintSource for (&Option<String>, &pr::Ty) {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        if let Some(label) = &self.0 {
            p.push(label)?;
            p.push(": ")?;
        }
        self.1.print(p)
    }

    fn span(&self) -> Option<crate::Span> {
        self.1.span()
    }
}
