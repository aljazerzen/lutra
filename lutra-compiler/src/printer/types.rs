use crate::pr;
use crate::printer::common::{Between, Separated};
use crate::printer::{PrintSource, Printer};

impl PrintSource for pr::Ty {
    #[tracing::instrument(name = "t", skip_all)]
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        tracing::trace!("ty {}", self.kind.as_ref());

        match &self.kind {
            pr::TyKind::Ident(ident) => p.push(ident.to_string())?,
            pr::TyKind::Primitive(prim) => p.push(prim.to_string())?,
            pr::TyKind::Tuple(fields) => {
                return Between {
                    prefix: "{",
                    node: &Separated {
                        nodes: fields,
                        sep_inline: ", ",
                        sep_line_end: ",",
                    },
                    suffix: "}",
                    span: self.span,
                }
                .print(p);
            }
            pr::TyKind::Array(item) => {
                return Between {
                    prefix: "[",
                    node: item.as_ref(),
                    suffix: "]",
                    span: self.span,
                }
                .print(p);
            }
            pr::TyKind::Enum(variants) => {
                p.push("enum ")?;

                Between {
                    prefix: "{",
                    node: &Separated {
                        nodes: variants,
                        sep_inline: ", ",
                        sep_line_end: ",",
                    },
                    suffix: "}",
                    span: self.span,
                }
                .print(p)?;
            }
            pr::TyKind::Func(func) => return print_ty_func(func, None, p),
            pr::TyKind::TupleComprehension(comp) => {
                p.push("{for ")?;
                p.push(pr::display_ident(&comp.variable_name))?;
                p.push(": ")?;
                p.push(pr::display_ident(&comp.variable_ty))?;
                p.push(" in ")?;
                comp.tuple.print(p)?;
                p.push(" do ")?;
                if let Some(name) = &comp.body_name {
                    p.push(pr::display_ident(name))?;
                    p.push(": ")?;
                }
                comp.body_ty.print(p)?;
                p.push("}")?;
            }
        };
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        self.span
    }
}

pub(super) fn print_ty_func<'c>(
    func: &pr::TyFunc,
    name: Option<&str>,
    p: &mut Printer<'c>,
) -> Option<()> {
    p.push("func ")?;
    if let Some(name) = name {
        p.push(pr::display_ident(name))?;
    }

    Between {
        prefix: "(",
        node: &Separated {
            nodes: &func.params,
            sep_inline: ", ",
            sep_line_end: ",",
        },
        suffix: ")",
        span: None,
    }
    .print(p)?;

    if let Some(return_ty) = &func.body {
        p.push(": ")?;
        return_ty.print(p)?;
    }
    if !func.ty_params.is_empty() {
        p.new_line();
        p.push("where ")?;

        p.indent();

        Separated {
            nodes: &func.ty_params,
            sep_inline: ", ",
            sep_line_end: ",",
        }
        .print(p)?;

        p.dedent();
    }

    Some(())
}

impl PrintSource for (Option<pr::Ty>, bool) {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        if self.1 {
            p.push("const ")?;
        }

        if let Some(ty) = &self.0 {
            ty.print(p)?;
        }

        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        self.0.as_ref().and_then(|t| t.span)
    }
}

impl PrintSource for pr::TyTupleField {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        if let Some(name) = &self.name {
            p.push(pr::display_ident(name))?;
            p.push(": ")?;
        }
        if self.unpack {
            p.push("..")?;
        }
        self.ty.print(p)
    }

    fn span(&self) -> Option<crate::Span> {
        self.ty.span
    }
}
impl PrintSource for pr::TyEnumVariant {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        p.push(pr::display_ident(&self.name))?;

        let is_unit = self.ty.kind.as_tuple().is_some_and(|f| f.is_empty());
        if is_unit {
            return Some(());
        }

        p.push(": ")?;
        self.ty.print(p)
    }

    fn span(&self) -> Option<crate::Span> {
        self.ty.span
    }
}

impl PrintSource for pr::TyParam {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        p.push(pr::display_ident(&self.name))?;
        match &self.domain {
            pr::TyParamDomain::Open => {}
            pr::TyParamDomain::OneOf(tys) => {
                p.push(": ")?;
                for (i, ty) in tys.iter().enumerate() {
                    if i > 0 {
                        p.push(" | ")?;
                    }
                    p.push(ty.to_string())?;
                }
            }
            pr::TyParamDomain::TupleHasFields(fields) => {
                p.push(": {")?;
                for (i, field) in fields.iter().enumerate() {
                    match &field.location {
                        pr::Lookup::Name(name) => {
                            p.push(pr::display_ident(name))?;
                            p.push(": ")?;
                        }
                        pr::Lookup::Position(p) => {
                            assert_eq!(i, *p as usize); // TODO: print these fields when they are out of order
                        }
                    }
                    field.ty.print(p)?;
                    p.push(", ")?;
                }
                p.push("..}")?;
            }
            pr::TyParamDomain::TupleLen { n } => {
                // not an actual Lutra syntax (there is no syntax for this)
                p.push(": {.. ")?;
                p.push(n.to_string())?;
                p.push(" ..}")?;
            }
            pr::TyParamDomain::EnumVariants(variants) => {
                p.push(": enum {")?;
                for (i, variant) in variants.iter().enumerate() {
                    if i > 0 {
                        p.push(", ")?;
                    }
                    p.push(pr::display_ident(&variant.name))?;
                    if variant.ty.kind.as_tuple().is_some_and(|f| f.is_empty()) {
                        p.push(": ")?;
                        variant.ty.print(p)?;
                    }
                }
                p.push(", ..}")?;
            }
        }
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        self.span
    }
}
