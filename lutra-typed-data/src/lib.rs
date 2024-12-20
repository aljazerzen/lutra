mod schema {
    include!(concat!(env!("OUT_DIR"), "/project.rs"));
}

use lutra_bin::{ir, Decode, Encode, Result, Value};

pub fn decode_typed_data(buffer: &[u8]) -> Result<(Value, ir::Ty)> {
    let typed_data = schema::TypedData::decode(buffer)?;

    let ty = type_to_ir(&typed_data.ty);

    let value = Value::decode(&typed_data.data, &ty)?;

    Ok((value, ty))
}

pub fn encode_typed_data(w: &mut impl std::io::Write, value: Value, ty: &ir::Ty) -> Result<()> {
    let data = value.encode(ty)?;

    let ty = type_from_pr(ty);

    let typed_data = schema::TypedData { ty, data };

    let mut buf = Vec::new();
    typed_data.encode(&mut buf)?;

    w.write_all(&buf)?;
    Ok(())
}

fn type_to_ir(ty: &schema::Ty) -> ir::Ty {
    let kind = match &ty.kind {
        schema::TyKind::Primitive(primitive) => {
            let primitive = match primitive {
                schema::PrimitiveSet::int8 => ir::PrimitiveSet::int8,
                schema::PrimitiveSet::int16 => ir::PrimitiveSet::int16,
                schema::PrimitiveSet::int32 => ir::PrimitiveSet::int32,
                schema::PrimitiveSet::int64 => ir::PrimitiveSet::int64,
                schema::PrimitiveSet::uint8 => ir::PrimitiveSet::uint8,
                schema::PrimitiveSet::uint16 => ir::PrimitiveSet::uint16,
                schema::PrimitiveSet::uint32 => ir::PrimitiveSet::uint32,
                schema::PrimitiveSet::uint64 => ir::PrimitiveSet::uint64,
                schema::PrimitiveSet::float32 => ir::PrimitiveSet::float32,
                schema::PrimitiveSet::float64 => ir::PrimitiveSet::float64,
                schema::PrimitiveSet::bool => ir::PrimitiveSet::bool,
                schema::PrimitiveSet::text => ir::PrimitiveSet::text,
            };
            ir::TyKind::Primitive(primitive)
        }
        schema::TyKind::Tuple(fields) => ir::TyKind::Tuple(
            fields
                .iter()
                .map(|f| {
                    let name = f.name.0.clone();
                    let ty = type_to_ir(&f.ty);
                    ir::TyTupleField { name, ty }
                })
                .collect(),
        ),
        schema::TyKind::Array(items_ty) => {
            ir::TyKind::Array(Box::new(type_to_ir(items_ty.as_ref())))
        }
        schema::TyKind::Enum(variants) => ir::TyKind::Enum(
            variants
                .iter()
                .map(|v| ir::TyEnumVariant {
                    name: v.name.clone(),
                    ty: type_to_ir(&v.ty),
                })
                .collect(),
        ),
    };

    let layout = ty.layout.as_ref().map(|layout| ir::TyLayout {
        head_size: layout.head_size,
        variants_recursive: layout.variants_recursive.clone(),
        body_ptrs: vec![], // TODO
    });

    ir::Ty {
        kind,
        name: None,
        layout,
    }
}

fn type_from_pr(ty: &ir::Ty) -> schema::Ty {
    let kind = match &ty.kind {
        ir::TyKind::Primitive(primitive) => {
            let primitive = match primitive {
                ir::PrimitiveSet::int8 => schema::PrimitiveSet::int8,
                ir::PrimitiveSet::int16 => schema::PrimitiveSet::int16,
                ir::PrimitiveSet::int32 => schema::PrimitiveSet::int32,
                ir::PrimitiveSet::int64 => schema::PrimitiveSet::int64,
                ir::PrimitiveSet::uint8 => schema::PrimitiveSet::uint8,
                ir::PrimitiveSet::uint16 => schema::PrimitiveSet::uint16,
                ir::PrimitiveSet::uint32 => schema::PrimitiveSet::uint32,
                ir::PrimitiveSet::uint64 => schema::PrimitiveSet::uint64,
                ir::PrimitiveSet::float32 => schema::PrimitiveSet::float32,
                ir::PrimitiveSet::float64 => schema::PrimitiveSet::float64,
                ir::PrimitiveSet::bool => schema::PrimitiveSet::bool,
                ir::PrimitiveSet::text => schema::PrimitiveSet::text,
            };
            schema::TyKind::Primitive(primitive)
        }
        ir::TyKind::Tuple(fields) => schema::TyKind::Tuple(
            fields
                .iter()
                .map(|f| {
                    let name = schema::OptText(f.name.clone());
                    let ty = type_from_pr(&f.ty);
                    schema::TyKindTupleItems { name, ty }
                })
                .collect(),
        ),
        ir::TyKind::Array(items_ty) => schema::TyKind::Array(Box::new(type_from_pr(items_ty))),
        ir::TyKind::Enum(variants) => schema::TyKind::Enum(
            variants
                .iter()
                .map(|v| schema::TyKindEnumItems {
                    name: v.name.clone(),
                    ty: type_from_pr(&v.ty),
                })
                .collect(),
        ),
        _ => todo!(),
    };
    let layout = ty.layout.as_ref().map(|layout| schema::TyLayout {
        head_size: layout.head_size,
        variants_recursive: layout.variants_recursive.clone(),
    });

    schema::Ty { kind, layout }
}
