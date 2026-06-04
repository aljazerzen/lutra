use lutra_bin::ir;

impl From<ir::Ty> for super::Ty {
    fn from(ty: ir::Ty) -> super::Ty {
        let kind = match ty.kind {
            ir::TyKind::Primitive(primitive) => {
                let primitive = match primitive {
                    ir::TyPrimitive::Prim8 => super::TyPrimitive::prim8,
                    ir::TyPrimitive::Prim16 => super::TyPrimitive::prim16,
                    ir::TyPrimitive::Prim32 => super::TyPrimitive::prim32,
                    ir::TyPrimitive::Prim64 => super::TyPrimitive::prim64,
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
