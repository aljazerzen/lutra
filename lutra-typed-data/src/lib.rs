mod schema {
    include!(concat!(env!("OUT_DIR"), "/project.rs"));
}

use lutra_bin::{Decode, Encode, Result, Value};
use lutra_frontend::pr;

pub fn decode_typed_data(buffer: &[u8]) -> Result<(Value, pr::Ty)> {
    let typed_data = schema::TypedData::decode_buffer(buffer)?;

    let ty = type_to_pr(&typed_data.ty);

    let mut data = Vec::with_capacity(typed_data.data.len());
    for datum in typed_data.data {
        data.push(datum as u8);
    }

    let value = Value::decode(&data, &ty)?;

    Ok((value, ty))
}

pub fn encode_typed_data(w: &mut impl std::io::Write, value: Value, ty: &pr::Ty) -> Result<()> {
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

fn type_to_pr(ty: &schema::Ty) -> pr::Ty {
    let kind = match &ty.kind {
        schema::TyKind::Primitive(primitive) => {
            let primitive = match primitive {
                schema::PrimitiveSet::int => pr::PrimitiveSet::Int,
                schema::PrimitiveSet::float => pr::PrimitiveSet::Float,
                schema::PrimitiveSet::bool => pr::PrimitiveSet::Bool,
                schema::PrimitiveSet::text => pr::PrimitiveSet::Text,
            };
            pr::TyKind::Primitive(primitive)
        }
        schema::TyKind::Tuple(fields) => pr::TyKind::Tuple(
            fields
                .iter()
                .map(|f| {
                    let name = f.name.0.clone();
                    let ty = type_to_pr(&f.ty);
                    pr::TyTupleField { name, ty }
                })
                .collect(),
        ),
        schema::TyKind::Array(items_ty) => {
            pr::TyKind::Array(Box::new(type_to_pr(items_ty.as_ref())))
        }
        schema::TyKind::Enum(variants) => pr::TyKind::Enum(
            variants
                .iter()
                .map(|v| pr::TyEnumVariant {
                    name: v.name.clone(),
                    ty: type_to_pr(&v.ty),
                })
                .collect(),
        ),
    };

    let layout = ty.layout.as_ref().map(|layout| pr::TyLayout {
        head_size: layout.head_size as usize,
        variants_recursive: layout
            .variants_recursive
            .iter()
            .map(|x| *x as usize)
            .collect(),
        body_ptr_offset: None, // TODO
    });

    pr::Ty {
        kind,
        span: None,
        name: None,
        layout,
    }
}

fn type_from_pr(ty: &pr::Ty) -> schema::Ty {
    let kind = match &ty.kind {
        pr::TyKind::Primitive(primitive) => {
            let primitive = match primitive {
                pr::PrimitiveSet::Int => schema::PrimitiveSet::int,
                pr::PrimitiveSet::Float => schema::PrimitiveSet::float,
                pr::PrimitiveSet::Bool => schema::PrimitiveSet::bool,
                pr::PrimitiveSet::Text => schema::PrimitiveSet::text,
                _ => todo!(),
            };
            schema::TyKind::Primitive(primitive)
        }
        pr::TyKind::Tuple(fields) => schema::TyKind::Tuple(
            fields
                .iter()
                .map(|f| {
                    let name = schema::OptText(f.name.clone());
                    let ty = type_from_pr(&f.ty);
                    schema::TyKindTupleItems { name, ty }
                })
                .collect(),
        ),
        pr::TyKind::Array(items_ty) => schema::TyKind::Array(Box::new(type_from_pr(items_ty))),
        pr::TyKind::Enum(variants) => schema::TyKind::Enum(
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
        head_size: layout.head_size as i64,
        variants_recursive: layout
            .variants_recursive
            .iter()
            .map(|x| *x as i64)
            .collect(),
    });

    schema::Ty { kind, layout }
}
