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
                    .map(|ty| super::TyFuncParam::simple(Some(ty)))
                    .collect(),
                body: Some(Box::new(super::Ty::from(func.body))),
                ty_params: Vec::new(),
            }),
            ir::TyKind::Ident(path) => super::TyKind::Ident(super::Path::new(path.0)),
        };

        let mut r = super::Ty::new(kind);
        r.name = ty.name;
        r
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
