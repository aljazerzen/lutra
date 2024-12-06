use crate::diagnostic::Diagnostic;
use crate::pr;
use crate::utils::fold;
use crate::utils::fold::PrFold;
use crate::Result;

pub fn run(module_def: pr::ModuleDef) -> Result<pr::ModuleDef> {
    Desugarator.fold_module_def(module_def)
}

struct Desugarator;

impl PrFold for Desugarator {
    fn fold_expr(&mut self, mut expr: pr::Expr) -> Result<pr::Expr> {
        expr.kind = match expr.kind {
            pr::ExprKind::Pipeline(p) => {
                let mut e = self.desugar_pipeline(p)?;
                e.alias = expr.alias.or(e.alias);
                return Ok(e);
            }
            pr::ExprKind::Range(r) => self.desugar_range(r)?,
            pr::ExprKind::Unary(unary) => self.desugar_unary(unary)?,
            pr::ExprKind::Binary(binary) => self.desugar_binary(binary)?,
            k => fold::fold_expr_kind(self, k)?,
        };
        Ok(expr)
    }
}

impl Desugarator {
    /// De-sugars range `a..b` into `{start=a, end=b}`.
    ///
    /// TODO: Open bounds are mapped into `null`.
    fn desugar_range(&mut self, v: pr::Range) -> Result<pr::ExprKind> {
        let mut start = fold::fold_optional_box(self, v.start)?
            .map(|b| *b)
            .unwrap_or_else(|| pr::Expr::new(pr::Literal::Integer(0))); // TODO
        start.alias = Some("start".into());

        let mut end = fold::fold_optional_box(self, v.end)?
            .map(|b| *b)
            .unwrap_or_else(|| pr::Expr::new(pr::Literal::Integer(10000))); // TODO
        end.alias = Some("end".into());
        Ok(pr::ExprKind::Tuple(vec![start, end]))
    }

    fn desugar_pipeline(&mut self, mut pipeline: pr::Pipeline) -> Result<pr::Expr> {
        let value = pipeline.exprs.remove(0);
        let mut value = self.fold_expr(value)?;

        for expr in pipeline.exprs {
            let mut expr = self.fold_expr(expr)?;
            let span = expr.span;

            match expr.kind {
                pr::ExprKind::FuncCall(mut func_call) => {
                    func_call.args.insert(0, value);
                    expr.kind = pr::ExprKind::FuncCall(func_call);

                    value = expr;
                }
                pr::ExprKind::Func(_) | pr::ExprKind::Ident(_) => {
                    let func = expr;

                    value = pr::Expr::new(pr::ExprKind::FuncCall(pr::FuncCall {
                        name: Box::new(func),
                        args: vec![value],
                    }));
                    value.span = span;
                }
                _ => {
                    return Err(Diagnostic::new_custom(
                        "pipeline can only contain function calls or functions",
                    )
                    .with_span(span))
                }
            };
        }

        Ok(value)
    }

    /// Desugar unary operators into function calls.
    fn desugar_unary(&mut self, pr::UnaryExpr { op, expr }: pr::UnaryExpr) -> Result<pr::ExprKind> {
        use pr::UnOp::*;

        let expr = self.fold_expr(*expr)?;

        let func_name = match op {
            Neg => ["std", "neg"],
            Not => ["std", "not"],
            Pos => return Ok(expr.kind),
        };
        Ok(pr::ExprKind::FuncCall(pr::FuncCall {
            name: Box::new(pr::Expr::new(pr::Path::new(func_name.to_vec()))),
            args: vec![expr],
        }))
    }

    /// Desugar binary operators into function calls.
    fn desugar_binary(
        &mut self,
        pr::BinaryExpr { op, left, right }: pr::BinaryExpr,
    ) -> Result<pr::ExprKind> {
        let left = self.fold_expr(*left)?;
        let right = self.fold_expr(*right)?;

        let func_name: Vec<&str> = match op {
            pr::BinOp::Mul => vec!["std", "mul"],
            pr::BinOp::DivInt => vec!["std", "div"],
            pr::BinOp::DivFloat => vec!["std", "div"], // TODO
            pr::BinOp::Mod => vec!["std", "mod"],
            pr::BinOp::Pow => vec!["std", "math", "pow"],
            pr::BinOp::Add => vec!["std", "add"],
            pr::BinOp::Sub => vec!["std", "sub"],
            pr::BinOp::Eq => vec!["std", "eq"],
            pr::BinOp::Ne => vec!["std", "ne"],
            pr::BinOp::Gt => vec!["std", "gt"],
            pr::BinOp::Lt => vec!["std", "lt"],
            pr::BinOp::Gte => vec!["std", "gte"],
            pr::BinOp::Lte => vec!["std", "lte"],
            pr::BinOp::RegexSearch => vec!["std", "regex_search"],
            pr::BinOp::And => vec!["std", "and"],
            pr::BinOp::Or => vec!["std", "or"],
            pr::BinOp::Coalesce => vec!["std", "coalesce"],
        };

        // For the power operator, we need to reverse the order, since `math.pow a
        // b` is equivalent to `b ** a`. (but for example `sub a b` is equivalent to
        // `a - b`).
        //
        // (I think this is the most globally consistent approach, since final
        // arguments should be the "data", which in the case of `pow` would be the
        // base; but it's not perfect, we could change it...)
        let (left, right) = match op {
            pr::BinOp::Pow => (right, left),
            _ => (left, right),
        };
        Ok(new_binop(left, &func_name, right).kind)
    }
}

fn new_binop(left: pr::Expr, op_name: &[&str], right: pr::Expr) -> pr::Expr {
    pr::Expr::new(pr::ExprKind::FuncCall(pr::FuncCall {
        name: Box::new(pr::Expr::new(pr::Path::new(op_name.to_vec()))),
        args: vec![left, right],
    }))
}
