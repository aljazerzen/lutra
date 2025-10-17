use crate::pr;
use crate::printer::common::{Between, Separated};
use crate::printer::{PrintSource, Printer};

impl PrintSource for pr::Expr {
    #[tracing::instrument(name = "e", skip_all)]
    fn print<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        tracing::trace!("expr {}", self.kind.as_ref());

        match &self.kind {
            pr::ExprKind::Ident(path) => p.push(path.to_string())?,
            pr::ExprKind::TupleLookup { base, lookup } => {
                p.merge(base.print(p.sub())?);
                p.push(".")?;
                match lookup {
                    pr::Lookup::Name(n) => p.push(n)?,
                    pr::Lookup::Position(i) => p.push(i.to_string())?,
                }
            }
            pr::ExprKind::Literal(literal) => p.push(literal.to_string())?,

            pr::ExprKind::TypeAnnotation(ann) => {
                p.merge(ann.expr.print(p.sub())?);
                p.push(": ")?;
                p.merge(ann.ty.print(p.sub())?);
            }
            pr::ExprKind::Range(range) => {
                if let Some(start) = &range.start {
                    p.merge(start.print(p.sub())?);
                }
                p.push("..")?;
                if let Some(end) = &range.end {
                    p.merge(end.print(p.sub())?);
                }
            }
            pr::ExprKind::Binary(binary) => {
                let (first, operations) = flatten_binary(binary);

                let mut nodes = Vec::with_capacity(operations.len() + 1);
                nodes.push((None, first));
                nodes.extend(operations.into_iter().map(|(o, e)| (Some(o), e)));

                return Separated {
                    nodes: &nodes,
                    sep_inline: " ",
                    sep_line_end: "",
                }
                .print(p);
            }
            pr::ExprKind::Unary(unary) => {
                p.push(unary.op.to_string())?;
                p.merge(unary.expr.print(p.sub())?);
            }

            pr::ExprKind::Nested(expr) => {
                return Between {
                    prefix: "(",
                    node: expr.as_ref(),
                    suffix: ")",
                    span: self.span,
                }
                .print(p);
            }
            pr::ExprKind::Tuple(fields) => {
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
            pr::ExprKind::Array(exprs) => {
                return Between {
                    prefix: "[",
                    node: &Separated {
                        nodes: exprs,
                        sep_inline: ", ",
                        sep_line_end: ",",
                    },
                    suffix: "]",
                    span: self.span,
                }
                .print(p);
            }

            pr::ExprKind::FuncCall(call) => {
                p.merge(call.func.print(p.sub())?);
                p.merge(
                    Between {
                        prefix: "(",
                        node: &Separated {
                            nodes: &call.args,
                            sep_inline: ", ",
                            sep_line_end: ",",
                        },
                        suffix: ")",
                        span: self.span,
                    }
                    .print(p.sub())?,
                );
            }

            pr::ExprKind::Func(func) => return print_func(func, None, p),
            pr::ExprKind::FString(interpolate_items) => {
                p.push("f\"")?;
                for item in interpolate_items {
                    match item {
                        pr::InterpolateItem::String(s) => {
                            p.push(pr::escape_all_except_quotes(s))?;
                        }
                        pr::InterpolateItem::Expr { expr, format } => {
                            p.push("{")?;
                            p.merge(expr.print(p.sub())?);
                            if let Some(format) = format {
                                p.push(":")?;
                                p.push(format)?;
                            }
                            p.push("}")?;
                        }
                    };
                }
                p.push("\"")?;
            }
            pr::ExprKind::Match(match_) => {
                if p.single_line {
                    return None;
                }

                p.push("match ")?;
                p.merge(match_.subject.print(p.sub())?);
                p.push(" {")?;
                p.indent();
                for branch in &match_.branches {
                    p.new_line();
                    p.merge(branch.print(p.sub())?);
                    p.push(",")?;
                }
                p.dedent();
                p.new_line();
                p.push("}")?;
            }
            pr::ExprKind::If(if_) => {
                p.push("if ")?;
                p.merge(if_.condition.print(p.sub())?);
                p.push(" then ")?;
                p.merge(if_.then.print(p.sub())?);
                p.push(" else ")?;
                p.merge(if_.els.print(p.sub())?);
            }

            pr::ExprKind::EnumVariant(_) | pr::ExprKind::Internal => unreachable!(),
        }
        Some(p)
    }

    fn span(&self) -> Option<crate::Span> {
        self.span
    }
}

fn flatten_binary(binary: &pr::BinaryExpr) -> (&pr::Expr, Vec<(pr::BinOp, &pr::Expr)>) {
    let mut first = binary.left.as_ref();
    let mut operations = vec![(binary.op, binary.right.as_ref())];

    while let pr::ExprKind::Binary(binary) = &first.kind {
        first = binary.left.as_ref();
        operations.push((binary.op, binary.right.as_ref()));
    }
    operations.reverse();

    let mut i = 0;
    while i < operations.len() {
        if let pr::ExprKind::Binary(binary) = &operations[i].1.kind {
            operations[i].1 = binary.left.as_ref();
            operations.insert(i + 1, (binary.op, binary.right.as_ref()));
        } else {
            i += 1;
        }
    }

    (first, operations)
}

impl PrintSource for (Option<pr::BinOp>, &pr::Expr) {
    fn print<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        if let Some(op) = self.0 {
            p.push(op.to_string())?;
            p.push(" ")?;
        }
        self.1.print(p)
    }

    fn span(&self) -> Option<crate::Span> {
        self.1.span
    }
}

pub(super) fn print_func<'c>(
    func: &pr::Func,
    name: Option<&str>,
    mut p: Printer<'c>,
) -> Option<Printer<'c>> {
    p.push("func ")?;

    if let Some(name) = name {
        p.push(pr::display_ident(name))?;
    }

    p.merge(
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
        .print(p.sub())?,
    );
    if let Some(return_ty) = &func.return_ty {
        p.push(": ")?;
        p.merge(return_ty.print(p.sub())?);
    }

    if !func.ty_params.is_empty() {
        if p.single_line {
            return None;
        }

        p.new_line();
        p.push("where ")?;

        p.indent();
        p.merge(
            Separated {
                nodes: &func.ty_params,
                sep_inline: ", ",
                sep_line_end: ",",
            }
            .print(p.sub())?,
        );
        p.dedent();

        p.new_line();
        p.push("-> ")?;
    } else {
        p.push(" -> ")?;
    }

    p.merge(func.body.print(p.sub())?);
    Some(p)
}

impl PrintSource for pr::TupleField {
    fn print<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        if let Some(name) = &self.name {
            let name = pr::display_ident(name);
            p.push(name)?;
            p.push(" = ")?;
        }

        if self.unpack {
            p.push("..")?;
        }

        p.merge(self.expr.print(p.sub())?);
        Some(p)
    }

    fn span(&self) -> Option<crate::Span> {
        self.expr.span
    }
}

impl PrintSource for pr::FuncParam {
    fn print<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        if self.constant {
            p.push("const ")?;
        }

        let name = pr::display_ident(&self.name);
        p.push(name)?;

        if let Some(ty) = &self.ty {
            p.push(": ")?;
            p.merge(ty.print(p.sub())?);
        }

        Some(p)
    }

    fn span(&self) -> Option<crate::Span> {
        Some(self.span)
    }
}

impl PrintSource for pr::MatchBranch {
    fn print<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        p.merge(self.pattern.print(p.sub())?);
        p.push(" => ")?;
        p.merge(self.value.print(p.sub())?);
        Some(p)
    }

    fn span(&self) -> Option<crate::Span> {
        Some(self.pattern.span)
    }
}

impl PrintSource for pr::Pattern {
    #[tracing::instrument(name = "p", skip_all)]
    fn print<'c>(&self, mut p: Printer<'c>) -> Option<Printer<'c>> {
        tracing::trace!("pattern {}", self.kind.as_ref());

        match &self.kind {
            pr::PatternKind::Enum(name, inner) => {
                p.push(".")?;
                p.push(pr::display_ident(name))?;
                if let Some(inner) = inner {
                    p.push("(")?;
                    p.merge(inner.print(p.sub())?);
                    p.push(")")?;
                }
            }
            pr::PatternKind::Literal(literal) => p.push(literal.to_string())?,
            pr::PatternKind::AnyOf(patterns) => {
                return Separated {
                    nodes: patterns,
                    sep_inline: " | ",
                    sep_line_end: " |",
                }
                .print(p);
            }
            pr::PatternKind::Bind(name) => p.push(pr::display_ident(name))?,
        };
        Some(p)
    }

    fn span(&self) -> Option<crate::Span> {
        Some(self.span)
    }
}
