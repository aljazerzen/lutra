use lutra_bin::ir;

impl From<ir::Ty> for super::Ty {
    fn from(ty: ir::Ty) -> super::Ty {
        let kind = match ty.kind {
            ir::TyKind::Primitive(primitive) => {
                let primitive = match primitive {
                    ir::PrimitiveSet::int => super::PrimitiveSet::int,
                    ir::PrimitiveSet::float => super::PrimitiveSet::float,
                    ir::PrimitiveSet::bool => super::PrimitiveSet::bool,
                    ir::PrimitiveSet::text => super::PrimitiveSet::text,
                };
                super::TyKind::Primitive(primitive)
            }
            ir::TyKind::Tuple(fields) => super::TyKind::Tuple(
                fields
                    .into_iter()
                    .map(|f| {
                        let name = f.name.clone();
                        let ty = super::Ty::from(f.ty);
                        super::TyTupleField { name, ty }
                    })
                    .collect(),
            ),
            ir::TyKind::Array(items_ty) => {
                super::TyKind::Array(Box::new(super::Ty::from(*items_ty)))
            }
            ir::TyKind::Enum(variants) => super::TyKind::Enum(
                variants
                    .into_iter()
                    .map(|v| super::TyEnumVariant {
                        name: v.name,
                        ty: super::Ty::from(v.ty),
                    })
                    .collect(),
            ),
            ir::TyKind::Function(func) => super::TyKind::Function(Some(super::TyFunc {
                params: func
                    .params
                    .into_iter()
                    .map(super::Ty::from)
                    .map(Some)
                    .collect(),
                body: Some(Box::new(super::Ty::from(func.body))),
            })),
            ir::TyKind::Ident(_) => todo!(),
        };

        let layout = ty.layout.map(|layout| super::TyLayout {
            head_size: layout.head_size as usize,
            variants_recursive: layout
                .variants_recursive
                .iter()
                .map(|x| *x as usize)
                .collect(),
            body_ptr_offset: layout.body_ptr_offset.map(|x| x as usize),
        });

        super::Ty {
            kind,
            span: None,
            name: ty.name,
            layout,
        }
    }
}

impl From<super::Ty> for ir::Ty {
    fn from(ty: super::Ty) -> ir::Ty {
        let kind = match ty.kind {
            super::TyKind::Primitive(primitive) => {
                let primitive = match primitive {
                    super::PrimitiveSet::int => ir::PrimitiveSet::int,
                    super::PrimitiveSet::float => ir::PrimitiveSet::float,
                    super::PrimitiveSet::bool => ir::PrimitiveSet::bool,
                    super::PrimitiveSet::text => ir::PrimitiveSet::text,
                };
                ir::TyKind::Primitive(primitive)
            }
            super::TyKind::Tuple(fields) => ir::TyKind::Tuple(
                fields
                    .into_iter()
                    .map(|f| {
                        let name = f.name.clone();
                        let ty = ir::Ty::from(f.ty);
                        ir::TyTupleField { name, ty }
                    })
                    .collect(),
            ),
            super::TyKind::Array(items_ty) => ir::TyKind::Array(Box::new(ir::Ty::from(*items_ty))),
            super::TyKind::Enum(variants) => ir::TyKind::Enum(
                variants
                    .into_iter()
                    .map(|v| ir::TyEnumVariant {
                        name: v.name,
                        ty: ir::Ty::from(v.ty),
                    })
                    .collect(),
            ),
            super::TyKind::Ident(path) => ir::TyKind::Ident(ir::Path(path.into_iter().collect())),
            super::TyKind::Function(_) => todo!(),
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
