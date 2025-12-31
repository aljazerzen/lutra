use crate::pr;
use crate::printer::common::{Between, Separated};
use crate::printer::{PrintSource, Printer};

impl PrintSource for pr::Expr {
    #[tracing::instrument(name = "e", skip_all)]
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        tracing::trace!("expr {}", self.kind.as_ref());

        match &self.kind {
            pr::ExprKind::Ident(path) => p.push(path.to_string())?,
            pr::ExprKind::Lookup { base, lookup } => {
                base.print(p)?;
                p.push(".")?;
                match lookup {
                    pr::Lookup::Name(n) => p.push(pr::display_ident(n))?,
                    pr::Lookup::Position(i) => p.push(i.to_string())?,
                }
            }
            pr::ExprKind::Literal(literal) => p.push(literal.to_string())?,

            pr::ExprKind::TypeAnnotation(ann) => {
                ann.expr.print(p)?;
                p.push(": ")?;
                ann.ty.print(p)?;
            }
            pr::ExprKind::Range(range) => {
                if let Some(start) = &range.start {
                    start.print(p)?;
                }
                p.push("..")?;
                if let Some(end) = &range.end {
                    end.print(p)?;
                }
            }
            pr::ExprKind::Binary(binary) => {
                let nodes = flatten_binary(binary);
                let nodes = Separated {
                    nodes: &nodes,
                    sep_inline: " ",
                    sep_line_end: "",
                };

                // try printing without parenthesis
                let mut inline = p.fork();
                inline.require_single_line(self.span)?;
                if nodes.print(&mut inline).is_some() {
                    p.merge(inline);
                    return Some(());
                }

                // inject parenthesis
                p.push("(")?;
                p.indent();
                p.new_line();
                nodes.print(p)?;
                p.dedent();
                p.new_line();
                p.push(")")?;
            }
            pr::ExprKind::Unary(unary) => {
                p.push(unary.op.to_string())?;
                unary.expr.print(p)?;
            }

            pr::ExprKind::Nested(expr) => {
                // special case
                if let pr::ExprKind::Binary(binary) = &expr.kind {
                    // when inner binary op, don't do a normal pass,
                    // because that might inject parenthesis

                    let nodes = flatten_binary(binary);
                    let nodes = Separated {
                        nodes: &nodes,
                        sep_inline: " ",
                        sep_line_end: "",
                    };
                    return Between {
                        prefix: "(",
                        node: &nodes,
                        suffix: ")",
                        span: self.span,
                    }
                    .print(p);
                }

                // general case
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

            pr::ExprKind::Call(call) => {
                call.subject.print(p)?;

                if call.args.len() <= 1 {
                    // special case: when there is only one arg, allow it to
                    // span multiples lines, without adding extra newlines at
                    // the start or end of parenthesis. For example:
                    //   map(x -> [
                    //     ... # many lines
                    //   ])
                    p.push("(")?;
                    if let Some(arg) = call.args.first() {
                        arg.print(p)?;
                    }
                    p.push(")")?;
                } else {
                    // general case
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
                    .print(p)?;
                }
            }

            pr::ExprKind::Func(func) => return print_func(func, None, p),

            pr::ExprKind::FuncShort(func) => {
                p.push(&func.param.name)?;
                p.push(" -> ")?;
                func.body.print(p)?;
            }

            pr::ExprKind::FString(interpolate_items) => {
                p.push("f\"")?;
                for item in interpolate_items {
                    match item {
                        pr::InterpolateItem::String(s) => {
                            p.push(pr::escape_all_except_quotes(s))?;
                        }
                        pr::InterpolateItem::Expr { expr, format } => {
                            p.push("{")?;
                            expr.print(p)?;
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
                match_.subject.print(p)?;
                p.push(" {")?;
                p.indent();
                for branch in &match_.branches {
                    p.new_line();
                    branch.print(p)?;
                    p.push(",")?;
                }
                p.dedent();
                p.new_line();
                p.push("}")?;
            }
            pr::ExprKind::If(if_) => {
                // try inline
                let mut inline = p.fork();
                if print_if_inline(if_, self.span, &mut inline).is_some() {
                    p.merge(inline);
                    return Some(());
                }

                p.push("if ")?;
                if_.condition.print(p)?;
                p.push(" then ")?;
                print_block(&if_.then, p)?;
                p.push(" else ")?;
                print_block(&if_.els, p)?;
            }

            pr::ExprKind::VarBinding(binding) => {
                if p.single_line {
                    return None;
                }

                p.push("let ")?;
                p.push(pr::display_ident(&binding.name))?;
                p.push(" = ")?;
                binding.bound.print(p)?;
                p.push(";")?;
                p.inject_trivia_prev_inline(binding.main.span.as_ref().map(|s| s.start));
                p.new_line();

                p.inject_trivia_leading(binding.main.span.as_ref().map(|s| s.start));

                binding.main.print(p)?;
            }

            pr::ExprKind::Variant(_) | pr::ExprKind::Native => unreachable!(),
        }
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        self.span
    }
}

/// Converts a possible deeply nested tree of binary operations into a flat list
/// of nodes.
fn flatten_binary(binary: &pr::BinaryExpr) -> Vec<(Option<pr::BinOp>, &pr::Expr)> {
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

    let mut nodes = Vec::with_capacity(operations.len() + 1);
    nodes.push((None, first));
    nodes.extend(operations.into_iter().map(|(o, e)| (Some(o), e)));
    nodes
}

fn print_if_inline<'c>(if_: &pr::If, span: Option<crate::Span>, p: &mut Printer<'c>) -> Option<()> {
    p.require_single_line(span)?;

    p.push("if ")?;
    if_.condition.print(p)?;
    p.push(" then ")?;
    if_.then.print(p)?;
    p.push(" else ")?;
    if_.els.print(p)?;

    Some(())
}

fn print_block<'c>(node: &pr::Expr, p: &mut Printer<'c>) -> Option<()> {
    p.push("(")?;

    p.indent();
    p.new_line();

    if let pr::ExprKind::Nested(unwrapped) = &node.kind {
        unwrapped.print(p)?;
    } else {
        node.print(p)?;
    }

    p.dedent();
    p.new_line();
    p.push(")")?;
    Some(())
}

impl PrintSource for (Option<pr::BinOp>, &pr::Expr) {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
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

    if let Some(return_ty) = &func.return_ty {
        p.push(": ")?;
        return_ty.print(p)?;
    }

    if !func.ty_params.is_empty() {
        if p.single_line {
            return None;
        }

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

        p.new_line();
        p.push("-> ")?;
    } else {
        p.push(" -> ")?;
    }

    func.body.print(p)?;
    Some(())
}

impl PrintSource for pr::TupleField {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        if let Some(name) = &self.name {
            let name = pr::display_ident(name);
            p.push(name)?;
            p.push(" = ")?;
        }

        if self.unpack {
            p.push("..")?;
        }

        self.expr.print(p)?;
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        self.expr.span
    }
}

impl PrintSource for pr::FuncParam {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        if self.constant {
            p.push("const ")?;
        }

        let name = pr::display_ident(&self.name);
        p.push(name)?;

        if let Some(ty) = &self.ty {
            p.push(": ")?;
            ty.print(p)?;
        }

        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        Some(self.span)
    }
}

impl PrintSource for pr::MatchBranch {
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        self.pattern.print(p)?;
        p.push(" => ")?;
        self.value.print(p)?;
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        Some(self.pattern.span)
    }
}

impl PrintSource for pr::Pattern {
    #[tracing::instrument(name = "p", skip_all)]
    fn print<'c>(&self, p: &mut Printer<'c>) -> Option<()> {
        tracing::trace!("pattern {}", self.kind.as_ref());

        match &self.kind {
            pr::PatternKind::Enum(name, inner) => {
                p.push(".")?;
                p.push(pr::display_ident(name))?;
                if let Some(inner) = inner {
                    p.push("(")?;
                    inner.print(p)?;
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
        Some(())
    }

    fn span(&self) -> Option<crate::Span> {
        Some(self.span)
    }
}
