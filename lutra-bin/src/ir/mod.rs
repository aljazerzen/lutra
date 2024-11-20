mod generated;
mod literal;
mod sid;

pub use generated::*;
pub use sid::SidKind;

pub use mapper::{ty_from_pr, ty_into_pr};

mod mapper {
    use super::generated as ir;
    use lutra_frontend::pr;

    pub fn ty_into_pr(ty: ir::Ty) -> pr::Ty {
        let kind = match ty.kind {
            ir::TyKind::Primitive(primitive) => {
                let primitive = match primitive {
                    ir::PrimitiveSet::int => pr::PrimitiveSet::int,
                    ir::PrimitiveSet::float => pr::PrimitiveSet::float,
                    ir::PrimitiveSet::bool => pr::PrimitiveSet::bool,
                    ir::PrimitiveSet::text => pr::PrimitiveSet::text,
                };
                pr::TyKind::Primitive(primitive)
            }
            ir::TyKind::Tuple(fields) => pr::TyKind::Tuple(
                fields
                    .into_iter()
                    .map(|f| {
                        let name = f.name.clone();
                        let ty = ty_into_pr(f.ty);
                        pr::TyTupleField { name, ty }
                    })
                    .collect(),
            ),
            ir::TyKind::Array(items_ty) => pr::TyKind::Array(Box::new(ty_into_pr(*items_ty))),
            ir::TyKind::Enum(variants) => pr::TyKind::Enum(
                variants
                    .into_iter()
                    .map(|v| pr::TyEnumVariant {
                        name: v.name,
                        ty: ty_into_pr(v.ty),
                    })
                    .collect(),
            ),
            ir::TyKind::Function(func) => pr::TyKind::Function(Some(pr::TyFunc {
                params: func.params.into_iter().map(ty_into_pr).map(Some).collect(),
                body: Some(Box::new(ty_into_pr(func.body))),
            })),
            ir::TyKind::Ident(_) => todo!(),
        };

        let layout = ty.layout.map(|layout| {
            let mut head_size = layout.head_size as usize;
            if head_size == 0 {
                if let Some(h) = kind.get_layout_simple() {
                    head_size = h.head_size;
                }
            }

            pr::TyLayout {
                head_size,
                variants_recursive: layout
                    .variants_recursive
                    .iter()
                    .map(|x| *x as usize)
                    .collect(),
                body_ptr_offset: None, // TODO
            }
        });

        pr::Ty {
            kind,
            span: None,
            name: ty.name,
            layout,
        }
    }

    pub fn ty_from_pr(ty: pr::Ty) -> ir::Ty {
        let kind = match ty.kind {
            pr::TyKind::Primitive(primitive) => {
                let primitive = match primitive {
                    pr::PrimitiveSet::int => ir::PrimitiveSet::int,
                    pr::PrimitiveSet::float => ir::PrimitiveSet::float,
                    pr::PrimitiveSet::bool => ir::PrimitiveSet::bool,
                    pr::PrimitiveSet::text => ir::PrimitiveSet::text,
                };
                ir::TyKind::Primitive(primitive)
            }
            pr::TyKind::Tuple(fields) => ir::TyKind::Tuple(
                fields
                    .into_iter()
                    .map(|f| {
                        let name = f.name.clone();
                        let ty = ty_from_pr(f.ty);
                        ir::TyTupleField { name, ty }
                    })
                    .collect(),
            ),
            pr::TyKind::Array(items_ty) => ir::TyKind::Array(Box::new(ty_from_pr(*items_ty))),
            pr::TyKind::Enum(variants) => ir::TyKind::Enum(
                variants
                    .into_iter()
                    .map(|v| ir::TyEnumVariant {
                        name: v.name,
                        ty: ty_from_pr(v.ty),
                    })
                    .collect(),
            ),
            pr::TyKind::Ident(path) => ir::TyKind::Ident(ir::Path(path.into_iter().collect())),
            pr::TyKind::Function(_) => todo!(),
        };

        let layout = ty.layout.map(|layout| ir::TyLayout {
            head_size: layout.head_size as i64,
            variants_recursive: layout
                .variants_recursive
                .iter()
                .map(|x| *x as i64)
                .collect(),
            body_ptr_offset: layout.body_ptr_offset.map(|o| o as i64),
        });

        ir::Ty {
            kind,
            layout,
            name: ty.name,
        }
    }
}
