use lutra_bin::ir;

use crate::sql::{
    cr::{self, CrFold},
    utils,
};

pub fn optimize(expr: cr::Expr) -> cr::Expr {
    Optimizer {}.fold_expr(expr).unwrap()
}

struct Optimizer {}

impl cr::CrFold for Optimizer {
    fn fold_expr(&mut self, expr: cr::Expr) -> Result<cr::Expr, ()> {
        let mut expr = cr::fold_expr(self, expr)?;
        expr = simplify_retain(expr);
        expr = simplify_discard(expr);
        expr = unpack_pack(expr);
        expr = pack_unpack(expr);

        let (e, re_fold) = push_correlated_into_group(expr);
        expr = e;

        if re_fold {
            expr = cr::fold_expr(self, expr)?;
        }

        Ok(expr)
    }
}

fn unpack_pack(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::From(cr::From::JsonUnpack(packed)) = &expr.kind else {
        return expr;
    };
    let mut packed = packed.as_ref();
    if let cr::ExprKind::From(cr::From::Row(row)) = &packed.kind
        && row.len() == 1
    {
        packed = &row[0];
    };
    let cr::ExprKind::From(cr::From::JsonPack(_)) = &packed.kind else {
        return expr;
    };
    tracing::debug!("unpack_pack");

    // unpack
    let cr::ExprKind::From(cr::From::JsonUnpack(packed)) = expr.kind else {
        unreachable!()
    };
    let mut packed = *packed;
    if let cr::ExprKind::From(cr::From::Row(row)) = packed.kind {
        packed = row.into_iter().next().unwrap();
    }
    let cr::ExprKind::From(cr::From::JsonPack(inner)) = packed.kind else {
        unreachable!()
    };

    *inner
}

fn pack_unpack(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::From(cr::From::JsonPack(unpacked)) = &expr.kind else {
        return expr;
    };
    let mut unpacked = unpacked.as_ref();
    if let cr::ExprKind::From(cr::From::Row(row)) = &unpacked.kind
        && row.len() == 1
    {
        unpacked = &row[0];
    };
    let cr::ExprKind::From(cr::From::JsonUnpack(_)) = &unpacked.kind else {
        return expr;
    };
    tracing::debug!("unpack_pack");

    // unpack
    let cr::ExprKind::From(cr::From::JsonPack(unpacked)) = expr.kind else {
        unreachable!()
    };
    let mut unpacked = *unpacked;
    if let cr::ExprKind::From(cr::From::Row(row)) = unpacked.kind {
        unpacked = row.into_iter().next().unwrap();
    }
    let cr::ExprKind::From(cr::From::JsonUnpack(inner)) = unpacked.kind else {
        unreachable!()
    };

    *inner
}

fn simplify_retain(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectRetain(_)) = &expr.kind else {
        return expr;
    };
    let cr::ExprKind::From(cr::From::Row(_)) = &bound.rel.kind else {
        return expr;
    };
    tracing::debug!("simplify_retain");

    // unpack
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectRetain(retain)) = expr.kind else {
        unreachable!();
    };
    let mut new_expr = bound.rel.kind;
    let cr::ExprKind::From(cr::From::Row(row)) = &mut new_expr else {
        unreachable!();
    };

    utils::retain_by_position(row, &retain);
    cr::Expr {
        kind: new_expr,
        ty: expr.ty,
    }
}

fn simplify_discard(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectDiscard(_)) = &expr.kind else {
        return expr;
    };
    let cr::ExprKind::From(cr::From::Row(_)) = &bound.rel.kind else {
        return expr;
    };
    tracing::debug!("simplify_discard");

    // unpack
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectDiscard(discard)) = expr.kind else {
        unreachable!();
    };
    let mut new_expr = bound.rel.kind;
    let cr::ExprKind::From(cr::From::Row(row)) = &mut new_expr else {
        unreachable!();
    };

    utils::drop_by_position(row, &discard);
    cr::Expr {
        kind: new_expr,
        ty: expr.ty,
    }
}

fn push_correlated_into_group(expr: cr::Expr) -> (cr::Expr, bool) {
    // match
    let cr::ExprKind::BindCorrelated(bound, correlated) = &expr.kind else {
        return (expr, false);
    };
    let cr::ExprKind::Transform(_, cr::Transform::Group(_, _)) = &bound.rel.kind else {
        return (expr, false);
    };
    let cr::ExprKind::From(cr::From::Row(correlated)) = &correlated.kind else {
        return (expr, false);
    };
    // does output contain the index?
    if !is_rel_col(&correlated[0], bound.id, 0) {
        return (expr, false);
    }
    tracing::debug!("push_correlated_into_group");

    // unpack
    let cr::ExprKind::BindCorrelated(bound, correlated) = expr.kind else {
        unreachable!()
    };
    let cr::ExprKind::From(cr::From::Row(correlated)) = correlated.kind else {
        unreachable!()
    };
    let mut new_expr = bound.rel;
    let cr::ExprKind::Transform(_, cr::Transform::Group(_, values)) = &mut new_expr.kind else {
        unreachable!()
    };

    let mut orig_values = std::mem::take(values);

    let mut correlated = correlated.into_iter();
    correlated.next().unwrap(); // remove index

    // fake index
    orig_values.insert(
        0,
        cr::Expr {
            kind: cr::ExprKind::From(cr::From::Null),
            ty: ir::Ty::new(ir::TyPrimitive::int64),
        },
    );
    let orig_value = cr::Expr {
        kind: cr::ExprKind::From(cr::From::Row(orig_values)),
        ty: new_expr.ty,
    };

    let mut replacer = RelRefReplacer::new(bound.id, orig_value);
    values.extend(correlated.map(|c| replacer.fold_expr(c).unwrap()));
    new_expr.ty = expr.ty;
    (new_expr, true)
}

fn is_rel_col(expr: &cr::Expr, rel_id: usize, col_pos: usize) -> bool {
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectRetain(retain)) = &expr.kind else {
        return false;
    };

    if retain != &[col_pos] {
        return false;
    }

    let cr::ExprKind::From(cr::From::RelRef(rel_ref)) = &bound.rel.kind else {
        return false;
    };

    *rel_ref == rel_id
}

pub struct RelRefReplacer {
    rel_ref: usize,
    replacement: cr::Expr,
}

impl RelRefReplacer {
    pub fn new(from_id: usize, to: cr::Expr) -> Self {
        RelRefReplacer {
            rel_ref: from_id,
            replacement: to,
        }
    }
}

impl CrFold for RelRefReplacer {
    fn fold_from(&mut self, from: cr::From, ty: ir::Ty) -> Result<cr::Expr, ()> {
        match from {
            cr::From::RelRef(id) if id == self.rel_ref => Ok(self.replacement.clone()),
            _ => cr::fold_from(self, from, ty),
        }
    }
}
