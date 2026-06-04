use std::{borrow::Cow, fmt::Write, iter::zip};

use lutra_bin::{ir, layout};
use lutra_compiler::pr;

use crate::rust::Context;

pub fn write_tys(
    w: &mut impl Write,
    tys: Vec<(ir::Ty, &[pr::Anno])>,
    ctx: &mut Context,
) -> Result<Vec<ir::Ty>, std::fmt::Error> {
    let mut all_tys = Vec::new();

    for (ty, annotations) in tys {
        if let Some(name) = ty.name.as_ref() {
            ctx.emitted_types.insert(name.clone());
        }
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
        // Skip types that have already been emitted.
        if let Some(name) = ty.name.as_ref()
            && !ctx.emitted_types.insert(name.clone())
        {
            continue;
        }
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
    annotations: &[pr::Anno],
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let lutra_bin = &ctx.options.lutra_bin_path;

    let name = ty.name.as_ref().unwrap();

    // derive traits
    let derive = (annotations.iter())
        .find_map(pr::Anno::as_std_rust_derive)
        .unwrap_or_else(|| vec!["Debug", "Clone"]);
    writeln!(w, "#[derive({})]", derive.join(", "))?;

    writeln!(w, "#[allow(non_camel_case_types)]")?;
    match &ty.kind {
        ir::TyKind::Primitive(_) | ir::TyKind::Array(_) | ir::TyKind::Ident(_) => {
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

        ir::TyKind::Enum(variants) if is_result_enum(variants, &ty.variants_recursive) => {
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
                let va_name = crate::snake_to_sentence(&variant.name);

                write!(w, "    {va_name}")?;
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

    if variants[index].ty.layout.is_none() {
        panic!("missing layout: {}\n{ty:#?}", lutra_bin::ir::print_ty(ty));
    }

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
        ir::TyKind::Primitive(ir::TyPrimitive::Prim8) => write!(w, "u8")?,
        ir::TyKind::Primitive(ir::TyPrimitive::Prim16) => write!(w, "u16")?,
        ir::TyKind::Primitive(ir::TyPrimitive::Prim32) => write!(w, "u32")?,
        ir::TyKind::Primitive(ir::TyPrimitive::Prim64) => write!(w, "u64")?,
        ir::TyKind::Ident(ident) => {
            if let Some(ty) = ty_ref_std(ident, ctx) {
                write!(w, "{ty}")?;
            } else {
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

        ir::TyKind::Enum(variants) if is_result_enum(variants, &ty.variants_recursive) => {
            let ok_ty = &variants[0].ty;
            let err_ty = &variants[1].ty;

            write!(w, "core::result::Result")?;
            if as_expr {
                write!(w, "::<")?;
            } else {
                write!(w, "<")?;
            }
            write_ty_ref(w, ok_ty, as_expr, ctx)?;
            write!(w, ", ")?;
            write_ty_ref(w, err_ty, as_expr, ctx)?;
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

fn ty_ref_std(ident: &ir::Path, ctx: &Context) -> Option<Cow<'static, str>> {
    let rust_ty = if ident.is(&["std", "Bool"]) {
        "bool"
    } else if ident.is(&["std", "Int8"]) {
        "i8"
    } else if ident.is(&["std", "Int16"]) {
        "i16"
    } else if ident.is(&["std", "Int32"]) {
        "i32"
    } else if ident.is(&["std", "Int64"]) {
        "i64"
    } else if ident.is(&["std", "Uint8"]) {
        "u8"
    } else if ident.is(&["std", "Uint16"]) {
        "u16"
    } else if ident.is(&["std", "Uint32"]) {
        "u32"
    } else if ident.is(&["std", "Uint64"]) {
        "u64"
    } else if ident.is(&["std", "Float32"]) {
        "f32"
    } else if ident.is(&["std", "Float64"]) {
        "f64"
    } else if ident.is(&["std", "Text"]) {
        return Some(Cow::Owned(format!(
            "{}::string::String",
            ctx.options.lutra_bin_path
        )));
    } else {
        return None;
    };
    Some(Cow::Borrowed(rust_ty))
}

pub fn is_unit_variant(variant_ty: &ir::Ty) -> bool {
    variant_ty.kind.as_tuple().is_some_and(|f| f.is_empty())
}

pub fn is_option_enum(variants: &[ir::TyEnumVariant]) -> bool {
    variants.len() == 2 && is_unit_variant(&variants[0].ty) && !is_unit_variant(&variants[1].ty)
}

pub fn is_result_enum(variants: &[ir::TyEnumVariant], variants_recursive: &[u16]) -> bool {
    variants_recursive.is_empty()
        && variants.len() == 2
        && !is_unit_variant(&variants[0].ty)
        && !is_unit_variant(&variants[1].ty)
}
