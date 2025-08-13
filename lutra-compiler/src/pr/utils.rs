use lutra_bin::ir;

impl From<ir::Ty> for super::Ty {
    fn from(ty: ir::Ty) -> super::Ty {
        let kind = match ty.kind {
            ir::TyKind::Primitive(primitive) => {
                let primitive = match primitive {
                    ir::TyPrimitive::bool => super::TyPrimitive::bool,
                    ir::TyPrimitive::int8 => super::TyPrimitive::int8,
                    ir::TyPrimitive::int16 => super::TyPrimitive::int16,
                    ir::TyPrimitive::int32 => super::TyPrimitive::int32,
                    ir::TyPrimitive::int64 => super::TyPrimitive::int64,
                    ir::TyPrimitive::uint8 => super::TyPrimitive::uint8,
                    ir::TyPrimitive::uint16 => super::TyPrimitive::uint16,
                    ir::TyPrimitive::uint32 => super::TyPrimitive::uint32,
                    ir::TyPrimitive::uint64 => super::TyPrimitive::uint64,
                    ir::TyPrimitive::float32 => super::TyPrimitive::float32,
                    ir::TyPrimitive::float64 => super::TyPrimitive::float64,
                    ir::TyPrimitive::text => super::TyPrimitive::text,
                };
                super::TyKind::Primitive(primitive)
            }
            ir::TyKind::Tuple(fields) => super::TyKind::Tuple(
                fields
                    .into_iter()
                    .map(|f| {
                        let name = f.name.clone();
                        let ty = super::Ty::from(f.ty);
                        super::TyTupleField {
                            name,
                            ty,
                            unpack: false,
                        }
                    })
                    .collect(),
            ),
            ir::TyKind::Array(items_ty) => {
                super::TyKind::Array(Box::new(super::Ty::from(*items_ty)))
            }
            ir::TyKind::Enum(variants) => super::TyKind::Enum(
                variants
                    .into_iter()
                    .map(super::TyEnumVariant::from)
                    .collect(),
            ),
            ir::TyKind::Function(func) => super::TyKind::Func(super::TyFunc {
                params: func
                    .params
                    .into_iter()
                    .map(super::Ty::from)
                    .map(Some)
                    .collect(),
                body: Some(Box::new(super::Ty::from(func.body))),
                ty_params: Vec::new(),
            }),
            ir::TyKind::Ident(path) => super::TyKind::Ident(super::Path::new(path.0)),
        };

        super::Ty {
            kind,
            span: None,
            name: ty.name,
            scope_id: None,
            target: None,
        }
    }
}

impl From<ir::TyEnumVariant> for super::TyEnumVariant {
    fn from(v: ir::TyEnumVariant) -> Self {
        super::TyEnumVariant {
            name: v.name,
            ty: super::Ty::from(v.ty),
        }
    }
}

impl From<super::Ty> for ir::Ty {
    fn from(ty: super::Ty) -> ir::Ty {
        let kind = match ty.kind {
            super::TyKind::Primitive(primitive) => {
                let primitive = match primitive {
                    super::TyPrimitive::int8 => ir::TyPrimitive::int8,
                    super::TyPrimitive::int16 => ir::TyPrimitive::int16,
                    super::TyPrimitive::int32 => ir::TyPrimitive::int32,
                    super::TyPrimitive::int64 => ir::TyPrimitive::int64,
                    super::TyPrimitive::uint8 => ir::TyPrimitive::uint8,
                    super::TyPrimitive::uint16 => ir::TyPrimitive::uint16,
                    super::TyPrimitive::uint32 => ir::TyPrimitive::uint32,
                    super::TyPrimitive::uint64 => ir::TyPrimitive::uint64,
                    super::TyPrimitive::float32 => ir::TyPrimitive::float32,
                    super::TyPrimitive::float64 => ir::TyPrimitive::float64,
                    super::TyPrimitive::bool => ir::TyPrimitive::bool,
                    super::TyPrimitive::text => ir::TyPrimitive::text,
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
            super::TyKind::Enum(variants) => {
                ir::TyKind::Enum(variants.into_iter().map(ir::TyEnumVariant::from).collect())
            }
            super::TyKind::Ident(path) => ir::TyKind::Ident(ir::Path(path.into_iter().collect())),
            super::TyKind::Func(func) => ir::TyKind::Function(Box::new(ir::TyFunction {
                params: func
                    .params
                    .into_iter()
                    .map(|p| ir::Ty::from(p.unwrap()))
                    .collect(),
                body: ir::Ty::from(*func.body.clone().unwrap()),
            })),
        };

        ir::Ty {
            kind,
            layout: None,
            name: ty.name,
            variants_recursive: vec![],
        }
    }
}

impl From<super::TyEnumVariant> for ir::TyEnumVariant {
    fn from(v: super::TyEnumVariant) -> Self {
        ir::TyEnumVariant {
            name: v.name,
            ty: ir::Ty::from(v.ty),
        }
    }
}
