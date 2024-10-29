mod schema {
    include!(concat!(env!("OUT_DIR"), "/schema.rs"));
}

use lutra_bin::{Decode, Encode, Result, Value};
use lutra_parser::parser::pr;
use schema::TyEnumItems;

pub fn decode_typed_data(buffer: &[u8]) -> Result<(Value<'static>, pr::Ty)> {
    let typed_data = schema::TypedData::decode_buffer(buffer)?;

    let ty = type_to_pr(&typed_data.ty);

    let mut data = Vec::with_capacity(typed_data.data.len());
    for datum in typed_data.data {
        data.push(datum as u8);
    }

    let value = Value::decode(&data, &ty)?;

    Ok((value.disown_type(), ty))
}

pub fn encode_typed_data<'t>(
    w: &mut impl std::io::Write,
    value: Value<'t>,
    ty: &'t pr::Ty,
) -> Result<()> {
    let mut data_u8 = Vec::new();
    value.encode(&mut data_u8, ty)?;

    let ty = type_from_pr(&ty);

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
    let kind = match ty {
        schema::Ty::Primitive(primitive) => {
            let primitive = match primitive {
                schema::PrimitiveSet::int => pr::PrimitiveSet::Int,
                schema::PrimitiveSet::float => pr::PrimitiveSet::Float,
                schema::PrimitiveSet::bool => pr::PrimitiveSet::Bool,
                schema::PrimitiveSet::text => pr::PrimitiveSet::Text,
            };
            pr::TyKind::Primitive(primitive)
        }
        schema::Ty::Tuple(fields) => pr::TyKind::Tuple(
            fields
                .into_iter()
                .map(|f| {
                    let name = match &f.name {
                        schema::OptText::None => None,
                        schema::OptText::Some(n) => Some(n.clone()),
                    };
                    let ty = type_to_pr(&f.ty);
                    pr::TyTupleField { name, ty }
                })
                .collect(),
        ),
        schema::Ty::Array(items_ty) => pr::TyKind::Array(Box::new(type_to_pr(&items_ty))),
        schema::Ty::Enum(variants) => pr::TyKind::Enum(
            variants
                .into_iter()
                .map(|v| (v.name.clone(), type_to_pr(&v.ty)))
                .collect(),
        ),
    };

    pr::Ty {
        kind,
        span: None,
        name: None,
    }
}

fn type_from_pr(ty: &pr::Ty) -> schema::Ty {
    match &ty.kind {
        pr::TyKind::Primitive(primitive) => {
            let primitive = match primitive {
                pr::PrimitiveSet::Int => schema::PrimitiveSet::int,
                pr::PrimitiveSet::Float => schema::PrimitiveSet::float,
                pr::PrimitiveSet::Bool => schema::PrimitiveSet::bool,
                pr::PrimitiveSet::Text => schema::PrimitiveSet::text,
                _ => todo!(),
            };
            schema::Ty::Primitive(primitive)
        }
        pr::TyKind::Tuple(fields) => schema::Ty::Tuple(
            fields
                .into_iter()
                .map(|f| {
                    let name = match &f.name {
                        None => schema::OptText::None,
                        Some(n) => schema::OptText::Some(n.clone()),
                    };
                    let ty = type_from_pr(&f.ty);
                    schema::TyTupleItems { name, ty }
                })
                .collect(),
        ),
        pr::TyKind::Array(items_ty) => schema::Ty::Array(Box::new(type_from_pr(&items_ty))),
        pr::TyKind::Enum(variants) => schema::Ty::Enum(
            variants
                .into_iter()
                .map(|v| TyEnumItems {
                    name: v.0.clone(),
                    ty: type_from_pr(&v.1),
                })
                .collect(),
        ),
        _ => todo!(),
    }
}
