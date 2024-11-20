mod schema {
    include!(concat!(env!("OUT_DIR"), "/project.rs"));
}

use lutra_bin::{ir, Decode, Encode, Result, Value};

pub fn decode_typed_data(buffer: &[u8]) -> Result<(Value, ir::Ty)> {
    let typed_data = schema::TypedData::decode_buffer(buffer)?;

    let ty = type_to_ir(&typed_data.ty);

    let mut data = Vec::with_capacity(typed_data.data.len());
    for datum in typed_data.data {
        data.push(datum as u8);
    }

    let value = Value::decode(&data, &ty)?;

    Ok((value, ty))
}

pub fn encode_typed_data(w: &mut impl std::io::Write, value: Value, ty: &ir::Ty) -> Result<()> {
    let data_u8 = value.encode(ty)?;

    let ty = type_from_pr(ty);

    let mut data = Vec::with_capacity(data_u8.len());
    for datum in data_u8 {
        data.push(datum as i64);
    }

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
                schema::PrimitiveSet::int => ir::PrimitiveSet::int,
                schema::PrimitiveSet::float => ir::PrimitiveSet::float,
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
        body_ptr_offset: None, // TODO
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
                ir::PrimitiveSet::int => schema::PrimitiveSet::int,
                ir::PrimitiveSet::float => schema::PrimitiveSet::float,
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
