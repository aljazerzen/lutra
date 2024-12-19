use std::{borrow::Cow, fmt::Write, iter::zip};

use lutra_bin::{ir, layout};
use lutra_frontend::pr;

use crate::Context;

pub fn write_tys(
    w: &mut impl Write,
    tys: Vec<(&String, ir::Ty, &Vec<pr::Annotation>)>,
    ctx: &mut Context,
) -> Result<Vec<ir::Ty>, std::fmt::Error> {
    let mut all_tys = Vec::new();

    for (name, mut ty, annotations) in tys {
        infer_names(name, &mut ty);

        write_ty_def(w, &ty, annotations, ctx)?;
        all_tys.push(ty);

        while let Some(ty) = ctx.def_buffer.pop_front() {
            let annotations = vec![];
            write_ty_def(w, &ty, &annotations, ctx)?;
            all_tys.push(ty);
        }
    }
    Ok(all_tys)
}

/// Types might not have names, because they are defined inline.
/// This function traverses a type definition and generates names for all of the types.
pub fn infer_names(stmt_name: &str, ty: &mut ir::Ty) {
    if ty.name.is_none() {
        ty.name = Some(stmt_name.to_string());
    }

    let mut name_prefix = Vec::new();
    infer_names_re(ty, &mut name_prefix);
}

fn infer_names_re(ty: &mut ir::Ty, name_prefix: &mut Vec<String>) {
    if ty.name.is_none() {
        ty.name = Some(name_prefix.concat());
    } else {
        name_prefix.push(ty.name.clone().unwrap());
    }

    match &mut ty.kind {
        ir::TyKind::Primitive(_) | ir::TyKind::Ident(_) => {}

        ir::TyKind::Tuple(fields) => {
            for (index, field) in fields.iter_mut().enumerate() {
                let name = tuple_field_name(&field.name, index);
                name_prefix.push(name.into_owned());

                infer_names_re(&mut field.ty, name_prefix);
                name_prefix.pop();
            }
        }

        ir::TyKind::Array(items_ty) => {
            name_prefix.push("Items".to_string());
            infer_names_re(items_ty, name_prefix);
            name_prefix.pop();
        }

        ir::TyKind::Enum(variants) => {
            for v in variants {
                name_prefix.push(v.name.clone());
                infer_names_re(&mut v.ty, name_prefix);
                name_prefix.pop();
            }
        }

        _ => unimplemented!(),
    }
}

/// Generates a type definition.
pub fn write_ty_def(
    w: &mut impl Write,
    ty: &ir::Ty,
    annotations: &[pr::Annotation],
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let name = ty.name.as_ref().unwrap();

    // derive traits
    let mut derive_traits = vec![];

    let derive_annotation = annotations.iter().find(|x| {
        x.expr
            .kind
            .as_func_call()
            .and_then(|c| c.name.kind.as_ident())
            .map_or(false, |i| i.name() == "derive")
    });
    if let Some(derive_annotation) = derive_annotation {
        let c = derive_annotation.expr.kind.as_func_call().unwrap();
        let values = c.args[0].kind.as_array().unwrap();
        derive_traits.extend(
            values
                .iter()
                .map(|e| e.kind.as_literal().unwrap().as_text().unwrap().clone()),
        );
    } else {
        derive_traits.extend(["Debug".into(), "Clone".into()]);
    }
    writeln!(w, "#[derive({})]", derive_traits.join(", "))?;

    writeln!(w, "#[allow(non_camel_case_types)]")?;
    match &ty.kind {
        ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
            // generate a wrapper new-type struct
            write!(w, "pub struct {}(pub ", name)?;
            write_ty_ref(w, ty, false, ctx)?;
            writeln!(w, ");\n")?;
        }

        ir::TyKind::Enum(variants) if is_option_enum(variants) => {
            // generate a wrapper new-type struct
            write!(w, "pub struct {}(pub ", name)?;
            write_ty_ref(w, ty, false, ctx)?;
            writeln!(w, ");\n")?;
        }

        ir::TyKind::Tuple(fields) => {
            writeln!(w, "pub struct {} {{", name)?;

            for (index, field) in fields.iter().enumerate() {
                let name = tuple_field_name(&field.name, index);

                write!(w, "    pub {name}: ")?;
                write_ty_ref(w, &field.ty, false, ctx)?;

                writeln!(w, ",")?;
            }

            writeln!(w, "}}\n")?;
        }

        ir::TyKind::Enum(variants) => {
            writeln!(w, "pub enum {} {{", name)?;

            for (index, variant) in variants.iter().enumerate() {
                write!(w, "    {}", variant.name)?;
                if !is_unit_variant(&variant.ty) {
                    let needs_box = layout::does_enum_variant_contain_recursive(ty, index as u16);

                    write!(w, "(")?;
                    if needs_box {
                        write!(w, "Box<")?;
                    }
                    write_ty_ref(w, &variant.ty, false, ctx)?;
                    if needs_box {
                        write!(w, ">")?;
                    }
                    write!(w, ")")?;
                }

                writeln!(w, ",")?;
            }

            writeln!(w, "}}\n")?;
        }

        _ => unimplemented!(),
    }

    Ok(())
}

pub fn tuple_field_name(name: &Option<String>, index: usize) -> Cow<'_, str> {
    (name.as_ref())
        .map(|x| Cow::Borrowed(x.as_str()))
        .unwrap_or_else(|| format!("field{index}").into())
}

/// Generates a reference to a type.
/// Syntactically, this could be used in `let x: type_ref` or `let x = Vec::<type_ref>::new();`.
/// The first example is not as-expr and the second one is.
pub fn write_ty_ref(
    w: &mut impl Write,
    ty: &ir::Ty,
    as_expr: bool,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    match &ty.kind {
        ir::TyKind::Primitive(ir::PrimitiveSet::bool) => {
            write!(w, "bool")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::int8) => {
            write!(w, "i8")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::int16) => {
            write!(w, "i16")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::int32) => {
            write!(w, "i32")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::int64) => {
            write!(w, "i64")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::uint8) => {
            write!(w, "u8")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::uint16) => {
            write!(w, "u16")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::uint32) => {
            write!(w, "u32")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::uint64) => {
            write!(w, "u64")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::float32) => {
            write!(w, "f32")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::float64) => {
            write!(w, "f64")?;
        }
        ir::TyKind::Primitive(ir::PrimitiveSet::text) => {
            write!(w, "String")?;
        }
        ir::TyKind::Ident(ident) => {
            let matching = zip(ident.0.iter(), ctx.current_module.iter())
                .filter(|(a, b)| a == b)
                .count();
            let supers = ctx.current_module.len() - matching;
            for _ in 0..supers {
                w.write_str("super::")?;
            }
            for (i, part) in ident.0.iter().skip(matching).enumerate() {
                if i > 0 {
                    w.write_str("::")?;
                }
                write!(w, "{part}")?;
            }
        }
        ir::TyKind::Array(items_ty) => {
            write!(w, "Vec")?;
            if as_expr {
                write!(w, "::<")?;
            } else {
                write!(w, "<")?;
            }
            write_ty_ref(w, items_ty, as_expr, ctx)?;
            write!(w, ">")?;
        }
        ir::TyKind::Enum(variants) if is_option_enum(variants) => {
            let inner_ty = &variants[1].ty;

            write!(w, "Option")?;
            if as_expr {
                write!(w, "::<")?;
            } else {
                write!(w, "<")?;
            }
            write_ty_ref(w, inner_ty, as_expr, ctx)?;
            write!(w, ">")?;
        }

        ir::TyKind::Tuple(_) | ir::TyKind::Enum(_) => {
            ctx.def_buffer.push_back(ty.clone());

            let name = ty.name.as_ref().unwrap();
            write!(w, "{name}")?;
        }

        _ => unimplemented!(),
    }
    Ok(())
}

pub fn is_unit_variant(variant_ty: &ir::Ty) -> bool {
    variant_ty.kind.as_tuple().map_or(false, |f| f.is_empty())
}

pub fn is_option_enum(variants: &[ir::TyEnumVariant]) -> bool {
    variants.len() == 2 && is_unit_variant(&variants[0].ty) && !is_unit_variant(&variants[1].ty)
}
