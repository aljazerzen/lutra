use std::{fmt::Write, iter::zip};

use lutra_bin::{ir, layout};
use lutra_compiler::pr;

use crate::rust::Context;

pub fn write_tys(
    w: &mut impl Write,
    tys: Vec<(ir::Ty, &[pr::Annotation])>,
    ctx: &mut Context,
) -> Result<Vec<ir::Ty>, std::fmt::Error> {
    let mut all_tys = Vec::new();

    for (ty, annotations) in tys {
        write_ty_def(w, &ty, annotations, ctx)?;
        all_tys.push(ty);

        all_tys.extend(write_tys_in_buffer(w, ctx)?);
    }
    Ok(all_tys)
}

pub fn write_tys_in_buffer(
    w: &mut impl Write,
    ctx: &mut Context<'_>,
) -> Result<Vec<ir::Ty>, std::fmt::Error> {
    let mut all_tys = Vec::new();
    while let Some(ty) = ctx.def_buffer.pop_front() {
        let annotations = vec![];
        write_ty_def(w, &ty, &annotations, ctx)?;

        all_tys.push(ty);
    }
    Ok(all_tys)
}

/// Generates a type definition.
pub fn write_ty_def(
    w: &mut impl Write,
    ty: &ir::Ty,
    annotations: &[pr::Annotation],
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let lutra_bin = &ctx.options.lutra_bin_path;

    let name = ty.name.as_ref().unwrap();

    // derive traits
    let mut derive_traits = vec![];

    let derive_annotation = annotations.iter().find(|x| {
        x.expr
            .kind
            .as_call()
            .and_then(|c| c.subject.kind.as_ident())
            .is_some_and(|i| i.last() == "derive")
    });
    if let Some(derive_annotation) = derive_annotation {
        let c = derive_annotation.expr.kind.as_call().unwrap();
        let values = c.args[0].expr.kind.as_array().unwrap();
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
            write!(w, "pub struct {name}(pub ")?;
            write_ty_ref(w, ty, false, ctx)?;
            writeln!(w, ");\n")?;
        }

        ir::TyKind::Enum(variants) if is_option_enum(variants) => {
            // generate a wrapper new-type struct
            write!(w, "pub struct {name}(pub ")?;
            write_ty_ref(w, ty, false, ctx)?;
            writeln!(w, ");\n")?;
        }

        ir::TyKind::Tuple(fields) => {
            writeln!(w, "pub struct {name} {{")?;

            for (index, field) in fields.iter().enumerate() {
                let name = crate::tuple_field_name(&field.name, index);

                write!(w, "    pub {name}: ")?;
                write_ty_ref(w, &field.ty, false, ctx)?;

                writeln!(w, ",")?;
            }

            writeln!(w, "}}\n")?;
        }

        ir::TyKind::Enum(variants) => {
            writeln!(w, "pub enum {name} {{")?;

            for (index, variant) in variants.iter().enumerate() {
                write!(w, "    {}", variant.name)?;
                if !is_unit_variant(&variant.ty) {
                    let needs_box = variant_needs_box(ty, index);

                    write!(w, "(")?;
                    if needs_box {
                        write!(w, "{lutra_bin}::boxed::Box<")?;
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

pub fn variant_needs_box(ty: &ir::Ty, index: usize) -> bool {
    let variants = ty.kind.as_enum().unwrap();

    let layout = variants[index].ty.layout.as_ref().unwrap();
    if layout.head_size > 200 {
        return true;
    }
    layout::does_enum_variant_contain_recursive(ty, index as u16)
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
    if ty.is_unit() {
        return write!(w, "()");
    }

    let lutra_bin = &ctx.options.lutra_bin_path;

    match &ty.kind {
        ir::TyKind::Primitive(ir::TyPrimitive::bool) => {
            write!(w, "bool")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::int8) => {
            write!(w, "i8")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::int16) => {
            write!(w, "i16")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::int32) => {
            write!(w, "i32")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::int64) => {
            write!(w, "i64")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::uint8) => {
            write!(w, "u8")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::uint16) => {
            write!(w, "u16")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::uint32) => {
            write!(w, "u32")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::uint64) => {
            write!(w, "u64")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::float32) => {
            write!(w, "f32")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::float64) => {
            write!(w, "f64")?;
        }
        ir::TyKind::Primitive(ir::TyPrimitive::text) => {
            write!(w, "{lutra_bin}::string::String")?;
        }
        ir::TyKind::Ident(ident) => {
            let matching = zip(ident.0.iter(), ctx.current_rust_mod.iter())
                .filter(|(a, b)| a == b)
                .count();
            let supers = ctx.current_rust_mod.len() - matching;
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
            write!(w, "{lutra_bin}::vec::Vec")?;
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

            write!(w, "core::option::Option")?;
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

            let name = ty
                .name
                .as_ref()
                .unwrap_or_else(|| panic!("no name for {ty:?}"));
            write!(w, "{name}")?;
        }

        _ => unimplemented!(),
    }
    Ok(())
}

pub fn is_unit_variant(variant_ty: &ir::Ty) -> bool {
    variant_ty.kind.as_tuple().is_some_and(|f| f.is_empty())
}

pub fn is_option_enum(variants: &[ir::TyEnumVariant]) -> bool {
    variants.len() == 2 && is_unit_variant(&variants[0].ty) && !is_unit_variant(&variants[1].ty)
}
