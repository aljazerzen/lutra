use lutra_bin::ir;

impl From<ir::Ty> for super::Ty {
    fn from(ty: ir::Ty) -> super::Ty {
        let kind = match ty.kind {
            ir::TyKind::Primitive(primitive) => {
                let primitive = match primitive {
                    ir::PrimitiveSet::bool => super::PrimitiveSet::bool,
                    ir::PrimitiveSet::int8 => super::PrimitiveSet::int8,
                    ir::PrimitiveSet::int16 => super::PrimitiveSet::int16,
                    ir::PrimitiveSet::int32 => super::PrimitiveSet::int32,
                    ir::PrimitiveSet::int64 => super::PrimitiveSet::int64,
                    ir::PrimitiveSet::uint8 => super::PrimitiveSet::uint8,
                    ir::PrimitiveSet::uint16 => super::PrimitiveSet::uint16,
                    ir::PrimitiveSet::uint32 => super::PrimitiveSet::uint32,
                    ir::PrimitiveSet::uint64 => super::PrimitiveSet::uint64,
                    ir::PrimitiveSet::float32 => super::PrimitiveSet::float32,
                    ir::PrimitiveSet::float64 => super::PrimitiveSet::float64,
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
            head_size: layout.head_size,
            variants_recursive: layout.variants_recursive,
            body_ptr_offset: layout.body_ptr_offset,
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
                    super::PrimitiveSet::int8 => ir::PrimitiveSet::int8,
                    super::PrimitiveSet::int16 => ir::PrimitiveSet::int16,
                    super::PrimitiveSet::int32 => ir::PrimitiveSet::int32,
                    super::PrimitiveSet::int64 => ir::PrimitiveSet::int64,
                    super::PrimitiveSet::uint8 => ir::PrimitiveSet::uint8,
                    super::PrimitiveSet::uint16 => ir::PrimitiveSet::uint16,
                    super::PrimitiveSet::uint32 => ir::PrimitiveSet::uint32,
                    super::PrimitiveSet::uint64 => ir::PrimitiveSet::uint64,
                    super::PrimitiveSet::float32 => ir::PrimitiveSet::float32,
                    super::PrimitiveSet::float64 => ir::PrimitiveSet::float64,
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
            head_size: layout.head_size,
            variants_recursive: layout.variants_recursive,
            body_ptr_offset: layout.body_ptr_offset,
        });

        ir::Ty {
            kind,
            layout,
            name: ty.name,
        }
    }
}
