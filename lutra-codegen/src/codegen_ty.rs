use std::{borrow::Cow, collections::VecDeque, fmt::Write};

use lutra_bin::layout;
use lutra_frontend::pr;

pub fn write_tys(
    w: &mut impl Write,
    tys: &[(&String, &pr::Ty)],
) -> Result<Vec<pr::Ty>, std::fmt::Error> {
    let mut all_tys = Vec::new();

    let mut ctx = Context::default();
    for (name, ty) in tys {
        let mut ty = (*ty).clone();

        infer_names(name, &mut ty);

        write_ty_def(w, &ty, &mut ctx)?;
        all_tys.push(ty);

        while let Some(ty) = ctx.def_buffer.pop_front() {
            write_ty_def(w, &ty, &mut ctx)?;
            all_tys.push(ty);
        }
    }
    Ok(all_tys)
}

pub fn write_tys_impls(w: &mut impl Write, tys: &[pr::Ty]) -> Result<(), std::fmt::Error> {
    if tys.is_empty() {
        return Ok(());
    }

    writeln!(w, "mod impls {{")?;
    writeln!(w, "#![allow(unused_imports)]")?;
    writeln!(w, "use ::std::io::Write;\n")?;
    writeln!(w, "use super::*;")?;
    for ty in tys {
        write_ty_def_impl(w, ty)?;
    }
    writeln!(w, "}}")
}

/// Types might not have names, because they are defined inline.
/// This function traverses a type definition and generates names for all of the types.
pub fn infer_names(stmt_name: &str, ty: &mut pr::Ty) {
    if ty.name.is_none() {
        ty.name = Some(stmt_name.to_string());
    }

    let mut name_prefix = Vec::new();
    infer_names_re(ty, &mut name_prefix);
}

fn infer_names_re(ty: &mut pr::Ty, name_prefix: &mut Vec<String>) {
    if ty.name.is_none() {
        ty.name = Some(name_prefix.concat());
    } else {
        name_prefix.push(ty.name.clone().unwrap());
    }

    match &mut ty.kind {
        pr::TyKind::Primitive(_) | pr::TyKind::Ident(_) => {}

        pr::TyKind::Tuple(fields) => {
            for (index, field) in fields.iter_mut().enumerate() {
                let name = tuple_field_name(&field.name, index);
                name_prefix.push(name.into_owned());

                infer_names_re(&mut field.ty, name_prefix);
                name_prefix.pop();
            }
        }

        pr::TyKind::Array(items_ty) => {
            name_prefix.push("Items".to_string());
            infer_names_re(items_ty, name_prefix);
            name_prefix.pop();
        }

        pr::TyKind::Enum(variants) => {
            for (v_name, v_ty) in variants {
                name_prefix.push(v_name.clone());
                infer_names_re(v_ty, name_prefix);
                name_prefix.pop();
            }
        }

        _ => unimplemented!(),
    }
}

#[derive(Default)]
pub struct Context {
    /// Buffer for types that need their definitions generated.
    def_buffer: VecDeque<pr::Ty>,
}

/// Generates a type definition.
pub fn write_ty_def(
    w: &mut impl Write,
    ty: &pr::Ty,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let name = ty.name.as_ref().unwrap();

    // derive traits
    let mut traits = vec!["Debug", "Clone"];
    match &ty.kind {
        pr::TyKind::Primitive(pr::PrimitiveSet::Bool)
        | pr::TyKind::Primitive(pr::PrimitiveSet::Int)
        | pr::TyKind::Primitive(pr::PrimitiveSet::Float) => {
            traits.extend(["Copy", "PartialEq", "Eq", "Hash"])
        }
        _ => {}
    }
    writeln!(w, "#[derive({})]", traits.join(", "))?;

    writeln!(w, "#[allow(non_camel_case_types)]")?;
    match &ty.kind {
        pr::TyKind::Primitive(_) | pr::TyKind::Array(_) => {
            // generate a wrapper new-type struct
            write!(w, "pub struct {}(pub ", name)?;
            write_ty_ref(w, ty, false, ctx)?;
            writeln!(w, ");\n")?;
        }

        pr::TyKind::Tuple(fields) => {
            writeln!(w, "pub struct {} {{", name)?;

            for (index, field) in fields.iter().enumerate() {
                let name = tuple_field_name(&field.name, index);

                write!(w, "    pub {name}: ")?;
                write_ty_ref(w, &field.ty, false, ctx)?;

                writeln!(w, ",")?;
            }

            writeln!(w, "}}\n")?;
        }

        pr::TyKind::Enum(variants) => {
            writeln!(w, "pub enum {} {{", name)?;

            for (index, (variant_name, variant_ty)) in variants.iter().enumerate() {
                write!(w, "    {variant_name}")?;
                if !is_unit_variant(variant_ty) {
                    let needs_box = layout::does_enum_variant_contain_recursive(ty, index);

                    write!(w, "(")?;
                    if needs_box {
                        write!(w, "Box<")?;
                    }
                    write_ty_ref(w, variant_ty, false, ctx)?;
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

fn tuple_field_name(name: &Option<String>, index: usize) -> Cow<'_, str> {
    (name.as_ref())
        .map(|x| Cow::Borrowed(x.as_str()))
        .unwrap_or_else(|| format!("field{index}").into())
}

/// Generates a reference to a type.
/// Syntactically, this could be used in `let x: type_ref`.
fn write_ty_ref(
    w: &mut impl Write,
    ty: &pr::Ty,
    as_expr: bool,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    match &ty.kind {
        pr::TyKind::Primitive(pr::PrimitiveSet::Int) => {
            write!(w, "i64")?;
        }
        pr::TyKind::Primitive(pr::PrimitiveSet::Float) => {
            write!(w, "f64")?;
        }
        pr::TyKind::Primitive(pr::PrimitiveSet::Bool) => {
            write!(w, "bool")?;
        }
        pr::TyKind::Primitive(pr::PrimitiveSet::Text) => {
            write!(w, "String")?;
        }
        pr::TyKind::Ident(ident) => {
            write!(w, "{}", ident.name())?;
        }
        pr::TyKind::Array(items_ty) => {
            write!(w, "Vec")?;
            if as_expr {
                write!(w, "::<")?;
            } else {
                write!(w, "<")?;
            }
            write_ty_ref(w, items_ty, as_expr, ctx)?;
            write!(w, ">")?;
        }

        pr::TyKind::Tuple(_) | pr::TyKind::Enum(_) => {
            ctx.def_buffer.push_back(ty.clone());

            let name = ty.name.as_ref().unwrap();
            write!(w, "{name}")?;
        }

        _ => unimplemented!(),
    }
    Ok(())
}

/// Generates the impl encode for a type.
#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
fn write_ty_def_impl(w: &mut impl Write, ty: &pr::Ty) -> Result<(), std::fmt::Error> {
    let name = ty.name.as_ref().unwrap();

    match &ty.kind {
        pr::TyKind::Primitive(_) => {
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = ();")?;
            writeln!(w, "    fn encode_head(&self, w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            writeln!(w, "        self.0.encode_head(w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_body(&self, _: (), _w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            writeln!(w, "        Ok(())")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
        }

        pr::TyKind::Array(_) => {
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = ::lutra_bin::OffsetPointer;")?;
            writeln!(w, "    fn encode_head(&self, w: &mut Vec<u8>) -> ::lutra_bin::Result<Self::HeadPtr> {{")?;
            writeln!(w, "        self.0.encode_head(w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_body(&self, head: Self::HeadPtr, w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            writeln!(w, "        self.0.encode_body(head, w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
        }

        pr::TyKind::Tuple(fields) => {
            writeln!(w, "#[allow(clippy::all)]")?;
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = {name}HeadPtr;")?;

            // encode head
            writeln!(w, "    fn encode_head(&self, w: &mut Vec<u8>) -> ::lutra_bin::Result<Self::HeadPtr> {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "        let {0} = self.{0}.encode_head(w)?;", field_name)?;
            }
            writeln!(w, "        Ok({name}HeadPtr {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "            {field_name},")?;
            }
            writeln!(w, "        }})")?;
            writeln!(w, "    }}")?;

            // encode body
            writeln!(w, "    fn encode_body(&self, head: Self::HeadPtr, w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;

            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "        self.{0}.encode_body(head.{0}, w)?;", field_name)?;
            }
            writeln!(w, "        Ok(())")?;

            writeln!(w, "    }}")?;

            writeln!(w, "}}")?;

            // head ptr struct
            writeln!(w, "#[allow(non_camel_case_types)]")?;
            writeln!(w, "pub struct {name}HeadPtr {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                write!(w, "    {field_name}: <")?;

                let mut ctx = Context::default();
                write_ty_ref(w, &field.ty, true, &mut ctx)?;

                writeln!(w, " as ::lutra_bin::Encode>::HeadPtr,")?;
            }
            writeln!(w, "}}")?;
        }

        pr::TyKind::Enum(variants) => {
            let head = layout::enum_head_format(variants);

            let needs_head_ptr = variants.iter().any(|(_, t)| !is_unit_variant(t));
            let head_ptr_name = if needs_head_ptr {
                format!("{name}HeadPtr")
            } else {
                "()".to_string()
            };

            writeln!(w, "#[allow(unused_variables)]")?;
            writeln!(w, "#[allow(clippy::all)]")?;
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = {head_ptr_name};")?;
            writeln!(w, "    fn encode_head(&self, w: &mut Vec<u8>) -> ::lutra_bin::Result<{head_ptr_name}> {{")?;
            writeln!(w, "        Ok(match self {{")?;

            for (tag, (variant_name, variant_ty)) in variants.iter().enumerate() {
                let variant = layout::enum_variant_format(variant_ty);

                write!(w, "            Self::{variant_name}")?;

                if !variant.is_inline {
                    write!(w, "(_)")?;
                } else if !is_unit_variant(variant_ty) {
                    write!(w, "(inner)")?;
                }
                writeln!(w, " => {{")?;

                let tag_bytes = &tag.to_le_bytes()[0..head.s / 8];
                writeln!(w, "                w.write_all(&{tag_bytes:?})?;")?;

                if !variant.is_inline {
                    writeln!(w, "                let head_ptr = ::lutra_bin::OffsetPointer::new(w);")?;
                    writeln!(w, "                let r = {head_ptr_name}::{variant_name}(head_ptr);")?;
                } else if !is_unit_variant(variant_ty) {
                    writeln!(w, "                let inner_head_ptr = inner.encode_head(w)?;")?;
                    writeln!(w, "                let r = {head_ptr_name}::{variant_name}(inner_head_ptr);")?;
                } else if needs_head_ptr {
                    writeln!(w, "                let r = {head_ptr_name}::None;")?;
                }

                if variant.padding > 0 {
                    write!(w, "                w.write_all(&[")?;
                    for _ in 0..(variant.padding / 8) {
                        write!(w, "0u8,")?;
                    }
                    writeln!(w, "])?;")?;
                }
                if needs_head_ptr {
                    writeln!(w, "                r")?;
                }

                writeln!(w, "            }},")?;
            }
            writeln!(w, "        }})")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_body(&self, head: {head_ptr_name}, w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            if needs_head_ptr {
                writeln!(w, "        match self {{")?;

                for (variant_name, variant_ty) in variants {
                    write!(w, "            Self::{variant_name}")?;
                    if !is_unit_variant(variant_ty) {
                        write!(w, "(inner)")?;
                    }
                    writeln!(w, " => {{")?;

                    let variant = layout::enum_variant_format(variant_ty);

                    if !variant.is_inline {
                        writeln!(w, "                let {head_ptr_name}::{variant_name}(offset_ptr) = head else {{ unreachable!() }};")?;
                        writeln!(w, "                offset_ptr.write(w);")?;
                        writeln!(w, "                let inner_head_ptr = inner.encode_head(w)?;")?;
                        writeln!(w, "                inner.encode_body(inner_head_ptr, w)?;")?;
                    } else if !is_unit_variant(variant_ty) {
                        writeln!(w, "                let {head_ptr_name}::{variant_name}(inner_head_ptr) = head else {{ unreachable!() }};")?;
                        writeln!(w, "                inner.encode_body(inner_head_ptr, w)?;")?;
                    }

                    writeln!(w, "            }},")?;
                }

                writeln!(w, "        }}")?;
            }
            writeln!(w, "        Ok(())")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;

            if needs_head_ptr {
                writeln!(w, "#[allow(non_camel_case_types, dead_code)]")?;
                writeln!(w, "pub enum {head_ptr_name} {{")?;
                writeln!(w, "    None,")?;
                for (variant_name, variant_ty) in variants {
                    if is_unit_variant(variant_ty) {
                        continue;
                    }

                    let variant = layout::enum_variant_format(variant_ty);

                    write!(w, "    {variant_name}")?;

                    if !variant.is_inline {
                        write!(w, "(::lutra_bin::OffsetPointer)")?;
                    } else {
                        write!(w, "(<")?;
                        let mut ctx = Context::default();
                        write_ty_ref(w, variant_ty, false, &mut ctx)?;
                        write!(w, " as ::lutra_bin::Encode>::HeadPtr)")?;
                    }

                    writeln!(w, ",")?;
                }
                writeln!(w, "}}")?;
            }
        }

        _ => unimplemented!(),
    }

    let head_size = ty.layout.as_ref().unwrap().head_size;
    writeln!(w, "impl ::lutra_bin::Layout for {name} {{")?;
    writeln!(w, "    fn head_size() -> usize {{")?;
    writeln!(w, "        {head_size}")?;
    writeln!(w, "    }}")?;
    writeln!(w, "}}\n")?;

    match &ty.kind {
        pr::TyKind::Primitive(_) | pr::TyKind::Array(_) => {
            writeln!(w, "impl ::lutra_bin::Decode for {name} {{")?;
            writeln!(
                w,
                "    fn decode(r: &mut ::lutra_bin::Reader<'_>) -> ::lutra_bin::Result<Self> {{"
            )?;

            write!(w, "        Ok(Self(")?;
            let mut ctx = Context::default();
            write_ty_ref(w, ty, true, &mut ctx)?;
            writeln!(w, "::decode(r)?))")?;

            writeln!(w, "    }}")?;
            writeln!(w, "}}\n")?;
        }

        pr::TyKind::Tuple(fields) => {
            writeln!(w, "impl ::lutra_bin::Decode for {name} {{")?;
            writeln!(w, "    fn decode(r: &mut ::lutra_bin::Reader<'_>) -> ::lutra_bin::Result<Self> {{")?;

            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);
                let field_ty = &field.ty;

                write!(w, "        let {field_name} = ")?;

                let mut ctx = Context::default();
                write_ty_ref(w, field_ty, true, &mut ctx)?;

                writeln!(w, "::decode(r)?;")?;
            }

            writeln!(w, "        Ok({name} {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);
                writeln!(w, "            {field_name},")?;
            }
            writeln!(w, "        }})")?;

            writeln!(w, "    }}")?;
            writeln!(w, "}}\n")?;
        }

        pr::TyKind::Enum(variants) => {
            writeln!(w, "impl ::lutra_bin::Decode for {name} {{")?;
            writeln!(
                w,
                "    fn decode(r: &mut ::lutra_bin::Reader<'_>) -> ::lutra_bin::Result<Self> {{"
            )?;

            let head = layout::enum_head_format(variants);

            // tag
            writeln!(w, "        let mut tag_bytes = r.copy_n({}).to_vec();", head.s / 8)?;
            writeln!(w, "        tag_bytes.resize(8, 0);")?;
            writeln!(w, "        let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;")?;

            writeln!(w, "        Ok(match tag {{")?;
            for (index, (variant_name, variant_ty)) in variants.iter().enumerate() {
                writeln!(w, "            {index} => {{")?;

                let variant_format = layout::enum_variant_format(variant_ty);

                if variant_format.is_inline {
                    if !is_unit_variant(variant_ty) {
                        write!(w, "                let inner = ")?;
                        let mut ctx = Context::default();
                        write_ty_ref(w, variant_ty, true, &mut ctx)?;
                        writeln!(w, "::decode(r)?;")?;
                    }
                } else {
                    writeln!(w, "                let mut body = r.clone();")?;
                    writeln!(w, "                let offset = r.copy_const::<4>();")?;
                    writeln!(w, "                let offset = u32::from_le_bytes(offset);")?;
                    writeln!(w, "                body.skip(offset as usize);")?;

                    write!(w, "                let inner = ")?;
                    let mut ctx = Context::default();
                    write_ty_ref(w, variant_ty, true, &mut ctx)?;
                    writeln!(w, "::decode(&mut body)?;")?;
                }

                if variant_format.padding > 0 {
                    writeln!(w, "                r.skip({});", variant_format.padding / 8)?;
                }

                let needs_box = layout::does_enum_variant_contain_recursive(ty, index);

                if is_unit_variant(variant_ty) {
                    writeln!(w, "                {name}::{variant_name}")?;
                } else if needs_box {
                    writeln!(w, "                {name}::{variant_name}(Box::new(inner))")?;
                } else {
                    writeln!(w, "                {name}::{variant_name}(inner)")?;
                }

                writeln!(w, "            }},")?;
            }
            writeln!(w, "            _ => return Err(::lutra_bin::Error::InvalidData)")?;
            writeln!(w, "        }})")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}\n")?;
        }

        _ => unimplemented!(),
    }

    Ok(())
}

fn is_unit_variant(variant_ty: &pr::Ty) -> bool {
    variant_ty.kind.as_tuple().map_or(false, |f| f.is_empty())
}
