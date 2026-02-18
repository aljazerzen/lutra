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
        expr = simplify_pick_row(expr);
        expr = simplify_discard_row(expr);
        expr = simplify_pick_discard(expr);
        expr = simplify_row_pick(expr);
        expr = simplify_pick_pick(expr);
        expr = simplify_reindex_reindex(expr);
        expr = unpack_pack(expr);
        expr = pack_unpack(expr);

        let (e, re_fold) = push_correlated_into_group(expr);
        expr = e;

        expr = push_bind_into_update(expr);
        expr = bind_to_correlated(expr);

        if re_fold {
            expr = cr::fold_expr(self, expr)?;
        }

        Ok(expr)
    }
}

fn unpack_pack(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::From(cr::From::Deserialize(packed)) = &expr.kind else {
        return expr;
    };
    let mut packed = packed.as_ref();
    if let cr::ExprKind::From(cr::From::Row(row)) = &packed.kind
        && row.len() == 1
    {
        packed = &row[0];
    };
    let cr::ExprKind::From(cr::From::Serialize(_)) = &packed.kind else {
        return expr;
    };
    tracing::debug!("unpack_pack");

    // unpack
    let cr::ExprKind::From(cr::From::Deserialize(packed)) = expr.kind else {
        unreachable!()
    };
    let mut packed = *packed;
    if let cr::ExprKind::From(cr::From::Row(row)) = packed.kind {
        packed = row.into_iter().next().unwrap();
    }
    let cr::ExprKind::From(cr::From::Serialize(inner)) = packed.kind else {
        unreachable!()
    };

    *inner
}

fn pack_unpack(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::From(cr::From::Serialize(unpacked)) = &expr.kind else {
        return expr;
    };
    let mut unpacked = unpacked.as_ref();
    if let cr::ExprKind::From(cr::From::Row(row)) = &unpacked.kind
        && row.len() == 1
    {
        unpacked = &row[0];
    };
    let cr::ExprKind::From(cr::From::Deserialize(_)) = &unpacked.kind else {
        return expr;
    };
    tracing::debug!("unpack_pack");

    // unpack
    let cr::ExprKind::From(cr::From::Serialize(unpacked)) = expr.kind else {
        unreachable!()
    };
    let mut unpacked = *unpacked;
    if let cr::ExprKind::From(cr::From::Row(row)) = unpacked.kind {
        unpacked = row.into_iter().next().unwrap();
    }
    let cr::ExprKind::From(cr::From::Deserialize(inner)) = unpacked.kind else {
        unreachable!()
    };

    *inner
}

fn simplify_pick_row(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectPick(_)) = &expr.kind else {
        return expr;
    };
    let cr::ExprKind::From(cr::From::Row(_)) = &bound.rel.kind else {
        return expr;
    };
    tracing::debug!("simplify_pick_row");

    // unpack
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectPick(pick)) = expr.kind else {
        unreachable!();
    };
    let mut new_expr = bound.rel.kind;
    let cr::ExprKind::From(cr::From::Row(row)) = &mut new_expr else {
        unreachable!();
    };

    utils::pick_by_position(row, &pick);
    cr::Expr {
        kind: new_expr,
        ty: expr.ty,
    }
}

fn simplify_discard_row(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectDiscard(_)) = &expr.kind else {
        return expr;
    };
    let cr::ExprKind::From(cr::From::Row(_)) = &bound.rel.kind else {
        return expr;
    };
    tracing::debug!("simplify_discard_row");

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

fn simplify_pick_discard(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectPick(_)) = &expr.kind else {
        return expr;
    };
    let cr::ExprKind::Transform(_, cr::Transform::ProjectDiscard(_)) = &bound.rel.kind else {
        return expr;
    };
    tracing::debug!("simplify_pick_discard");

    // unpack
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectPick(pick)) = expr.kind else {
        unreachable!();
    };
    let cr::ExprKind::Transform(inner, cr::Transform::ProjectDiscard(discard)) = bound.rel.kind
    else {
        unreachable!();
    };

    let mut cols: Vec<_> = (0..256).collect(); // TODO: this works only for <256 cols
    utils::drop_by_position(&mut cols, &discard);
    utils::pick_by_position(&mut cols, &pick);
    cr::Expr {
        kind: cr::ExprKind::Transform(inner, cr::Transform::ProjectPick(cols)),
        ty: expr.ty,
    }
}

fn simplify_pick_pick(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectPick(_)) = &expr.kind else {
        return expr;
    };
    let cr::ExprKind::Transform(_, cr::Transform::ProjectPick(_)) = &bound.rel.kind else {
        return expr;
    };
    tracing::debug!("simplify_pick_pick");

    // unpack
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectPick(pick_outer)) = expr.kind else {
        unreachable!();
    };
    let mut rel = bound.rel;
    let cr::ExprKind::Transform(_, cr::Transform::ProjectPick(pick_inner)) = &mut rel.kind else {
        unreachable!();
    };

    utils::pick_by_position(pick_inner, &pick_outer);
    rel
}

fn simplify_reindex_reindex(expr: cr::Expr) -> cr::Expr {
    // match: IndexBy(IndexBy(bound, inner_key), outer_key)
    let cr::ExprKind::Transform(bound, cr::Transform::Reindex(_)) = &expr.kind else {
        return expr;
    };
    let cr::ExprKind::Transform(_, cr::Transform::Reindex(_)) = &bound.rel.kind else {
        return expr;
    };
    tracing::debug!("simplify_index_by_index_by");

    // unpack
    let mut expr = expr;
    let cr::ExprKind::Transform(inner, cr::Transform::Reindex(_)) = &mut expr.kind else {
        unreachable!();
    };
    let inner_rel = std::mem::replace(&mut inner.rel.kind, cr::ExprKind::From(cr::From::Null));
    let cr::ExprKind::Transform(bound, cr::Transform::Reindex(_)) = inner_rel else {
        unreachable!();
    };

    // use the outer expr, but replace the bound expr with the inner bound expr
    inner.rel = bound.rel;

    expr
}

fn simplify_row_pick(expr: cr::Expr) -> cr::Expr {
    // match
    let cr::ExprKind::From(cr::From::Row(row)) = &expr.kind else {
        return expr;
    };

    if row.is_empty() {
        return expr;
    }

    let mut inner = None;
    for item in row {
        // validate that each row item is a Pick
        let cr::ExprKind::Transform(i, cr::Transform::ProjectPick(_)) = &item.kind else {
            return expr;
        };

        // ... and that they pick from the same inner value
        if let Some(inner) = &inner {
            if *inner != i {
                return expr;
            }
        } else {
            inner = Some(i);
        }
    }

    tracing::debug!("simplify_row_pick");

    // unpack
    let cr::ExprKind::From(cr::From::Row(row)) = expr.kind else {
        unreachable!();
    };
    let mut inner = None;
    let mut pick = Vec::new();
    for item in row {
        let cr::ExprKind::Transform(i, cr::Transform::ProjectPick(r)) = item.kind else {
            unreachable!()
        };
        inner = inner.or(Some(i));
        pick.extend(r);
    }
    let inner = inner.unwrap();

    cr::Expr {
        kind: cr::ExprKind::Transform(inner, cr::Transform::ProjectPick(pick)),
        ty: expr.ty,
    }
}

fn push_correlated_into_group(expr: cr::Expr) -> (cr::Expr, bool) {
    // match
    let cr::ExprKind::BindCorrelated(bound, correlated) = &expr.kind else {
        return (expr, false);
    };
    let cr::ExprKind::Transform(_, cr::Transform::Group { .. }) = &bound.rel.kind else {
        return (expr, false);
    };
    let cr::ExprKind::From(cr::From::Row(correlated)) = &correlated.kind else {
        return (expr, false);
    };
    // does output contain the index?
    if !is_rel_col(&correlated[0], bound.id, 0) {
        return (expr, false);
    }
    tracing::debug!("push_correlated_into_group: {expr:#?}");

    // unpack
    let cr::ExprKind::BindCorrelated(bound, correlated) = expr.kind else {
        unreachable!()
    };
    let cr::ExprKind::From(cr::From::Row(correlated)) = correlated.kind else {
        unreachable!()
    };
    let mut group = bound.rel;
    let cr::ExprKind::Transform(_, cr::Transform::Group { values, .. }) = &mut group.kind else {
        unreachable!()
    };

    let mut correlated = correlated.into_iter();
    correlated.next().unwrap(); // remove index

    // inline group values into the correlated row
    // 1) construct bound_rel that we'll use in place of the original bound
    // 2) replace refs to the bound rel
    let mut group_outputs = vec![
        cr::Expr::null(ir::Ty::new(ir::TyPrimitive::int64)), // fake index
    ];
    group_outputs.append(values); // values
    let bound_rel = cr::Expr {
        kind: cr::ExprKind::From(cr::From::Row(group_outputs)),
        ty: group.ty,
    };
    let mut replacer = RelRefReplacer::new(bound.id, bound_rel);

    // replace Group.values with the correlated row
    values.extend(correlated.map(|c| replacer.fold_expr(c).unwrap()));

    // return group with input and keys unchanged
    group.ty = expr.ty;
    (group, true)
}

fn is_rel_col(expr: &cr::Expr, rel_id: usize, col_pos: usize) -> bool {
    let cr::ExprKind::Transform(bound, cr::Transform::ProjectPick(pick)) = &expr.kind else {
        return false;
    };

    if pick != &[col_pos] {
        return false;
    }

    let cr::ExprKind::From(cr::From::RelRef(rel_ref)) = &bound.rel.kind else {
        return false;
    };

    *rel_ref == rel_id
}

fn push_bind_into_update(expr: cr::Expr) -> cr::Expr {
    // match: Bind(val, Update(table, updates))
    let cr::ExprKind::Bind(_bound, main) = &expr.kind else {
        return expr;
    };
    let cr::ExprKind::Update { .. } = &main.kind else {
        return expr;
    };
    tracing::debug!("push_bind_into_update");

    // unpack
    let cr::ExprKind::Bind(bound, main) = expr.kind else {
        unreachable!()
    };
    let main_ty = main.ty;
    let cr::ExprKind::Update { table, updates } = main.kind else {
        unreachable!()
    };

    // Construct Update(table, Bind(bound, updates)
    cr::Expr {
        ty: main_ty,
        kind: cr::ExprKind::Update {
            table,
            updates: Box::new(cr::Expr {
                ty: updates.ty.clone(),
                kind: cr::ExprKind::Bind(bound, updates),
            }),
        },
    }
}

fn bind_to_correlated(expr: cr::Expr) -> cr::Expr {
    // match: Bind(val, main)
    let cr::ExprKind::Bind(bound, _main) = &expr.kind else {
        return expr;
    };

    // Check if bound expression is exactly one row
    let is_exactly_one_row = bound.rel.ty.kind.is_primitive()
        || bound.rel.ty.kind.is_tuple()
        || bound.rel.ty.kind.is_enum();

    if !is_exactly_one_row {
        return expr;
    }

    tracing::debug!("bind_to_correlated");

    // unpack
    let cr::ExprKind::Bind(bound, main) = expr.kind else {
        unreachable!()
    };

    // Convert Bind to BindCorrelated
    cr::Expr {
        ty: expr.ty,
        kind: cr::ExprKind::BindCorrelated(bound, main),
    }
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
