use indexmap::IndexMap;

use crate::diagnostic::Diagnostic;
use crate::pr::{self, Expr};
use crate::resolver::NS_STD;
use crate::utils::fold;
use crate::utils::fold::PrFold;
use crate::{Result, Span};

pub fn run(module_def: pr::ModuleDef) -> Result<pr::ModuleDef> {
    Desugarator.fold_module_def(module_def)
}

pub fn run_expr(expr: pr::Expr) -> Result<pr::Expr> {
    Desugarator.fold_expr(expr)
}

struct Desugarator;

impl PrFold for Desugarator {
    fn fold_module_def(&mut self, module_def: pr::ModuleDef) -> Result<pr::ModuleDef> {
        let mut defs = IndexMap::with_capacity(module_def.defs.len());
        for (name, def) in module_def.defs {
            // special case: imports
            if let pr::DefKind::Import(import) = def.kind {
                defs.extend(self.desugar_import(import));
                continue;
            }

            // base case
            defs.insert(name, self.fold_def(def)?);
        }
        Ok(pr::ModuleDef { defs })
    }

    fn fold_expr(&mut self, mut expr: pr::Expr) -> Result<pr::Expr> {
        expr.kind = match expr.kind {
            pr::ExprKind::Nested(p) => {
                // unwrap the Expr
                return self.fold_expr(*p);
            }
            pr::ExprKind::Range(r) => self.desugar_range(r)?,
            pr::ExprKind::Unary(unary) => self.desugar_unary(unary, expr.span.unwrap())?,

            pr::ExprKind::Binary(pr::BinaryExpr {
                op: pr::BinOp::Pipe,
                left,
                right,
            }) => {
                expr.span = right.span;
                expr.kind = self.desugar_pipeline(*left, *right)?;
                return Ok(expr);
            }
            pr::ExprKind::Binary(binary) => self.desugar_binary(binary, expr.span.unwrap())?,
            pr::ExprKind::FString(items) => self.desugar_f_string(items, expr.span.unwrap())?,
            pr::ExprKind::FuncShort(func) => self.desugar_func_short(*func)?,
            k => fold::fold_expr_kind(self, k)?,
        };
        Ok(expr)
    }
}

impl Desugarator {
    /// De-sugars range `a..b` into `{start=a, end=b}`.
    ///
    /// TODO: open bounds should be mapped into `null`.
    fn desugar_range(&mut self, v: pr::Range) -> Result<pr::ExprKind> {
        let start = fold::fold_optional_box(self, v.start)?
            .map(|b| *b)
            .unwrap_or_else(|| pr::Expr::new(pr::Literal::Integer(0))); // TODO

        let end = fold::fold_optional_box(self, v.end)?
            .map(|b| *b)
            .unwrap_or_else(|| pr::Expr::new(pr::Literal::Integer(10000))); // TODO

        Ok(pr::ExprKind::Tuple(vec![
            pr::TupleField {
                name: Some("start".into()),
                unpack: false,
                expr: start,
            },
            pr::TupleField {
                name: Some("end".into()),
                unpack: false,
                expr: end,
            },
        ]))
    }

    fn desugar_pipeline(&mut self, left: pr::Expr, right: pr::Expr) -> Result<pr::ExprKind> {
        let value = self.fold_expr(left)?;
        let func = self.fold_expr(right)?;
        match func.kind {
            pr::ExprKind::Call(mut func_call) => {
                func_call.args.insert(0, value);

                Ok(pr::ExprKind::Call(func_call))
            }
            pr::ExprKind::Func(_) | pr::ExprKind::Ident(_) => Ok(pr::ExprKind::Call(pr::Call {
                subject: Box::new(func),
                args: vec![value],
            })),
            _ => Err(
                Diagnostic::new_custom("pipe operator requires function calls or functions")
                    .with_span(func.span),
            ),
        }
    }

    /// Desugar unary operators into function calls.
    fn desugar_unary(
        &mut self,
        pr::UnaryExpr { op, expr }: pr::UnaryExpr,
        span: Span,
    ) -> Result<pr::ExprKind> {
        use pr::UnOp::*;

        let expr = self.fold_expr(*expr)?;

        let func_name = match op {
            Neg => [NS_STD, "neg"],
            Not => [NS_STD, "not"],
            Pos => return Ok(expr.kind),
        };
        let mut op_func = pr::Expr::new(pr::Path::new(func_name.to_vec()));
        op_func.span = Some(span);
        Ok(pr::ExprKind::Call(pr::Call {
            subject: Box::new(op_func),
            args: vec![expr],
        }))
    }

    /// Desugar binary operators into function calls.
    fn desugar_binary(
        &mut self,
        pr::BinaryExpr { op, left, right }: pr::BinaryExpr,
        span: Span,
    ) -> Result<pr::ExprKind> {
        if op == pr::BinOp::Pipe {
            return self.desugar_pipeline(*left, *right);
        }

        let left = self.fold_expr(*left)?;
        let right = self.fold_expr(*right)?;

        let func_name: Vec<&str> = match op {
            pr::BinOp::Mul => vec![NS_STD, "mul"],
            pr::BinOp::DivInt => vec![NS_STD, "div"],
            pr::BinOp::DivFloat => vec![NS_STD, "div"], // TODO
            pr::BinOp::Mod => vec![NS_STD, "mod"],
            pr::BinOp::Pow => vec![NS_STD, "math", "pow"],
            pr::BinOp::Add => vec![NS_STD, "add"],
            pr::BinOp::Sub => vec![NS_STD, "sub"],
            pr::BinOp::Eq => vec![NS_STD, "eq"],
            pr::BinOp::Ne => vec![NS_STD, "ne"],
            pr::BinOp::Gt => vec![NS_STD, "gt"],
            pr::BinOp::Lt => vec![NS_STD, "lt"],
            pr::BinOp::Gte => vec![NS_STD, "gte"],
            pr::BinOp::Lte => vec![NS_STD, "lte"],
            pr::BinOp::RegexSearch => vec![NS_STD, "regex_search"],
            pr::BinOp::And => vec![NS_STD, "and"],
            pr::BinOp::Or => vec![NS_STD, "or"],
            pr::BinOp::Coalesce => vec![NS_STD, "or_else"],
            pr::BinOp::Pipe => unreachable!(),
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
        Ok(new_binop(left, &func_name, right, Some(span)).kind)
    }

    /// Desugar f-string into function calls to std::text::concat
    fn desugar_f_string(
        &mut self,
        items: Vec<pr::InterpolateItem>,
        string_span: Span,
    ) -> Result<pr::ExprKind> {
        let mut items = items.into_iter().map(|item| match item {
            pr::InterpolateItem::String(string) => {
                Expr::new_with_span(pr::Literal::Text(string), string_span)
            }
            pr::InterpolateItem::Expr {
                expr,
                format: _format,
            } => *expr,
        });

        // take first
        let Some(mut expr) = items.next() else {
            return Ok(pr::ExprKind::Literal(pr::Literal::Text("".to_string())));
        };

        // concat with the following
        for item in items {
            let op_span = Some(item.span.unwrap());
            expr = new_binop(expr, &[NS_STD, "text", "concat"], item, op_span);
        }
        Ok(expr.kind)
    }

    /// Desugars `import x::{a, b as c} into:
    /// ```lt
    /// import x::a as a
    /// import x::b as c
    /// ```
    fn desugar_import(&self, import: pr::ImportDef) -> Vec<(String, pr::Def)> {
        desugar_import_re(&pr::Path::empty(), import)
    }

    fn desugar_func_short(&mut self, func: pr::FuncShort) -> Result<pr::ExprKind> {
        Ok(pr::ExprKind::Func(Box::new(pr::Func {
            params: vec![func.param],
            return_ty: None,
            body: Some(Box::new(self.fold_expr(*func.body)?)),
            ty_params: vec![],
        })))
    }
}

fn desugar_import_re(prefix: &pr::Path, import: pr::ImportDef) -> Vec<(String, pr::Def)> {
    match import.kind {
        pr::ImportKind::Single(path, alias) => {
            let name = alias.unwrap_or_else(|| path.last().to_string());
            let mut def = pr::Def::new(pr::DefKind::Import(pr::ImportDef {
                kind: pr::ImportKind::Single(path.prepend(prefix.clone()), None),
                span: import.span,
            }));
            def.span = Some(import.span);
            vec![(name, def)]
        }
        pr::ImportKind::Many(path, parts) => {
            let mut r = Vec::with_capacity(parts.len());

            let prefix = path.prepend(prefix.clone());
            for i in parts {
                r.extend(desugar_import_re(&prefix, i));
            }
            r
        }
    }
}

fn new_binop(left: pr::Expr, op_name: &[&str], right: pr::Expr, op_span: Option<Span>) -> pr::Expr {
    let mut op = pr::Expr::new(pr::Path::new(op_name.to_vec()));
    op.span = op_span;

    pr::Expr::new(pr::ExprKind::Call(pr::Call {
        subject: Box::new(op),
        args: vec![left, right],
    }))
}
