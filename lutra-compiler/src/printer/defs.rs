use lutra_bin::ident;

use crate::pr::{self, ImportDef};
use crate::printer::common::{PrintSourceExt, Separated};
use crate::printer::{CONFIG_NO_WRAP, PrintSource, Printer};
use crate::printer::{expr, types};

/// Produce a compact, single-line signature string of a definition,
/// suitable for display in an LSP hover popup.
/// Prints resolved types as type annotations.
///
/// Returns `None` for [`pr::DefKind::Import`] definitions, which have no
/// meaningful signature to display.
pub fn print_def_signature(name: &str, def: &pr::Def) -> Option<String> {
    let mut printer = Printer::new(&CONFIG_NO_WRAP, None);
    let p = &mut printer;

    match &def.kind {
        pr::DefKind::Expr(expr_def) if expr_def.constant => {
            p.push("const ")?;
            p.push(ident::display(name))?;
            if let Some(ty) = expr_def.value.ty.as_deref() {
                p.push(": ")?;
                ty.print(p)?;
            }

            let mut f = p.fork();
            f.single_line = true;
            if f.push(" = ").is_some() && expr_def.value.print(&mut f).is_some() {
                p.merge(f);
            }
        }
        pr::DefKind::Expr(expr_def) => {
            let func = expr_def.value.ty.as_ref()?.kind.as_func()?;
            types::print_ty_func(func, Some(name), p)?;
        }
        pr::DefKind::Ty(ty_def) => {
            p.push("type ")?;
            p.push(ident::display(name))?;
            ty_def.print(p)?;
        }
        pr::DefKind::Module(_) => {
            p.push("module ")?;
            p.push(ident::display(name))?;
        }
        pr::DefKind::Import(_) => return None,
        pr::DefKind::Anno(ann_def) => {
            p.push("anno ")?;
            p.push(ident::display(name))?;
            p.push("(")?;
            for (i, param) in ann_def.params.iter().enumerate() {
                if i > 0 {
                    p.push(", ")?;
                }
                param.print(p)?;
            }
            p.push(")")?;
        }
    }

    assert!(p.edits.is_empty());
    Some(printer.buffer)
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
            if try_print_doc_anno(ann, p, "#! ") {
                continue;
            }
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
                p.inject_trivia_prev_inline(def.span.map(|s| s.start));

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

            // print the def (and emit one edit chunk per definition)
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

/// Try to print an annotation as a doc comment.
/// Returns `true` if the annotation was printed.
fn try_print_doc_anno(ann: &pr::Anno, p: &mut Printer, prefix: &str) -> bool {
    let Some(content) = ann.as_std_doc() else {
        return false;
    };
    p.inject_trivia_leading(ann.expr.span.map(|s| s.start));
    for line in content.lines() {
        p.push_unchecked(prefix);
        p.push_unchecked(line);
        p.new_line();
    }
    if let Some(span) = &ann.expr.span {
        while p.take_trivia_new_line(span.end()).is_some() {}
    }
    true
}

fn print_or_skip<T: PrintSource>(t: &T, p: &mut Printer) {
    let span = t.span().unwrap();
    p.mark_printed_up_to(span.start);

    let mut normal_print = p.fork();
    if t.print(&mut normal_print).is_some() {
        p.merge(normal_print);
        p.finish_edit();
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
        for (i, ann) in self.1.annotations.iter().enumerate() {
            p.inject_trivia_leading(ann.expr.span.map(|s| s.start));

            if try_print_doc_anno(ann, p, "## ") {
                continue;
            }
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
                expr::print_func(func, Some(self.0), p)?;
            }
            pr::DefKind::Ty(ty_def) => {
                p.push("type ")?;
                p.push(ident::display(self.0))?;
                ty_def.print(p)?;
            }
            pr::DefKind::Import(import_def) => {
                p.push("import ")?;
                import_def.print(p)?;
            }
            pr::DefKind::Anno(ann_def) => {
                p.push("anno ")?;
                p.push(ident::display(self.0))?;
                p.push("(")?;
                for (i, param) in ann_def.params.iter().enumerate() {
                    if i > 0 {
                        p.push(", ")?;
                    }
                    param.print(p)?;
                }
                p.push(")")?;
            }
        }

        p.mark_printed(&self.1.span);
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        self.1.span
    }
}

impl PrintSource for pr::TyDef {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        if self.is_framed {
            p.push("(")?;
            (&self.framed_label, &self.ty).print(p)?;
            p.push(")")
        } else {
            p.push(": ")?;
            self.ty.print(p)
        }
    }

    fn span(&self) -> Option<crate::Span> {
        self.ty.span
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
                parts.sort_by(|a, b| a.kind.path().cmp(b.kind.path()));

                Separated {
                    nodes: &parts,
                    sep_inline: ", ",
                    sep_line_end: ",",
                }
                .between("(", ")", None)
                .single_line_end(",")
                .print(p)
            }
            pr::ImportKind::Star(path) => {
                p.push(path.to_string())?;
                p.push("::*")?;
                Some(())
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
