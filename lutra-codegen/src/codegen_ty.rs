use std::{borrow::Cow, collections::VecDeque, fmt::Write, iter::zip};

use lutra_bin::{ir, layout};
use lutra_frontend::pr;

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

pub fn write_tys_impls(
    w: &mut impl Write,
    tys: &[ir::Ty],
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    if tys.is_empty() {
        return Ok(());
    }

    writeln!(w, "mod impls {{")?;
    writeln!(w, "#![allow(unused_imports)]")?;
    writeln!(w, "use ::std::io::Write;\n")?;
    writeln!(w, "use super::*;")?;
    for ty in tys {
        write_ty_def_impl(w, ty, ctx)?;
    }

    ctx.def_buffer.clear(); // all defs must have been already generated

    writeln!(w, "}}")
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

#[derive(Debug)]
pub struct Context {
    current_module: pr::Path,

    /// Buffer for types that don't have their own Lutra decl, but need their own Rust decl.
    /// When such type ref is encountered, it is pushed into here and generated later.
    def_buffer: VecDeque<ir::Ty>,
}

impl Context {
    pub fn new(module_path: pr::Path) -> Self {
        Self {
            def_buffer: Default::default(),
            current_module: module_path,
        }
    }

    pub fn is_done(&self) -> bool {
        self.def_buffer.is_empty()
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

fn tuple_field_name(name: &Option<String>, index: usize) -> Cow<'_, str> {
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

/// Generates the impl encode for a type.
#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
fn write_ty_def_impl(
    w: &mut impl Write,
    ty: &ir::Ty,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let name = ty.name.as_ref().unwrap();

    match &ty.kind {
        ir::TyKind::Primitive(_) => {
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

        ir::TyKind::Array(_) => {
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = ::lutra_bin::ReversePointer;")?;
            writeln!(w, "    fn encode_head(&self, w: &mut Vec<u8>) -> ::lutra_bin::Result<Self::HeadPtr> {{")?;
            writeln!(w, "        self.0.encode_head(w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_body(&self, head: Self::HeadPtr, w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            writeln!(w, "        self.0.encode_body(head, w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
        }

        ir::TyKind::Enum(variants) if is_option_enum(variants) => {
            let inner_ty = &variants[1].ty;

            let mut inner_head_ptr = String::new();
            write_ty_ref(&mut inner_head_ptr, inner_ty, true, ctx)?;

            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = Option<Result<")?;
            writeln!(w, "         <{inner_head_ptr} as ::lutra_bin::Encode>::HeadPtr,")?;
            writeln!(w, "         ::lutra_bin::ReversePointer,")?;
            writeln!(w, "    >>;")?;
            writeln!(w, "    fn encode_head(&self, w: &mut Vec<u8>) -> ::lutra_bin::Result<Self::HeadPtr> {{")?;
            writeln!(w, "        self.0.encode_head(w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_body(&self, head: Self::HeadPtr, w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            writeln!(w, "        self.0.encode_body(head, w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
        }

        ir::TyKind::Tuple(fields) => {
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

                write_ty_ref(w, &field.ty, true, ctx)?;

                writeln!(w, " as ::lutra_bin::Encode>::HeadPtr,")?;
            }
            writeln!(w, "}}")?;
        }

        ir::TyKind::Enum(variants) => {
            let head = layout::enum_head_format(variants);

            let needs_head_ptr = variants.iter().any(|variant| !is_unit_variant(&variant.ty));
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

            for (tag, variant) in variants.iter().enumerate() {
                let va_format = layout::enum_variant_format(&variant.ty);

                write!(w, "            Self::{}", variant.name)?;

                if !va_format.is_inline {
                    write!(w, "(_)")?;
                } else if !is_unit_variant(&variant.ty) {
                    write!(w, "(inner)")?;
                }
                writeln!(w, " => {{")?;

                let tag_bytes = &tag.to_le_bytes()[0..head.s / 8];
                writeln!(w, "                w.write_all(&{tag_bytes:?})?;")?;

                if !va_format.is_inline {
                    writeln!(w, "                let head_ptr = ::lutra_bin::ReversePointer::new(w);")?;
                    writeln!(w, "                let r = {head_ptr_name}::{}(head_ptr);", variant.name)?;
                } else if !is_unit_variant(&variant.ty) {
                    writeln!(w, "                let inner_head_ptr = inner.encode_head(w)?;")?;
                    writeln!(w, "                let r = {head_ptr_name}::{}(inner_head_ptr);", variant.name)?;
                } else if needs_head_ptr {
                    writeln!(w, "                let r = {head_ptr_name}::None;")?;
                }

                if va_format.padding > 0 {
                    write!(w, "                w.write_all(&[")?;
                    for _ in 0..(va_format.padding / 8) {
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

                for variant in variants {
                    write!(w, "            Self::{}", variant.name)?;
                    if !is_unit_variant(&variant.ty) {
                        write!(w, "(inner)")?;
                    }
                    writeln!(w, " => {{")?;

                    let variant_format = layout::enum_variant_format(&variant.ty);

                    if !variant_format.is_inline {
                        writeln!(w, "                let {head_ptr_name}::{}(offset_ptr) = head else {{ unreachable!() }};", variant.name)?;
                        writeln!(w, "                offset_ptr.write_cur_len(w);")?;
                        writeln!(w, "                let inner_head_ptr = inner.encode_head(w)?;")?;
                        writeln!(w, "                inner.encode_body(inner_head_ptr, w)?;")?;
                    } else if !is_unit_variant(&variant.ty) {
                        writeln!(w, "                let {head_ptr_name}::{}(inner_head_ptr) = head else {{ unreachable!() }};", variant.name)?;
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
                for variant in variants {
                    if is_unit_variant(&variant.ty) {
                        continue;
                    }

                    let variant_format = layout::enum_variant_format(&variant.ty);

                    write!(w, "    {}", variant.name)?;

                    if !variant_format.is_inline {
                        write!(w, "(::lutra_bin::ReversePointer)")?;
                    } else {
                        write!(w, "(<")?;

                        write_ty_ref(w, &variant.ty, false, ctx)?;
                        write!(w, " as ::lutra_bin::Encode>::HeadPtr)")?;
                    }

                    writeln!(w, ",")?;
                }
                writeln!(w, "}}")?;
            }
        }

        _ => unimplemented!(),
    }

    let head_size = &ty
        .layout
        .as_ref()
        .unwrap_or_else(|| panic!("ty is missing layout: {ty:?}"))
        .head_size;
    writeln!(w, "impl ::lutra_bin::Layout for {name} {{")?;
    writeln!(w, "    fn head_size() -> usize {{")?;
    writeln!(w, "        {head_size}")?;
    writeln!(w, "    }}")?;
    writeln!(w, "}}\n")?;

    match &ty.kind {
        ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
            writeln!(w, "impl ::lutra_bin::Decode for {name} {{")?;
            writeln!(
                w,
                "    fn decode(r: &mut ::lutra_bin::Reader<'_>) -> ::lutra_bin::Result<Self> {{"
            )?;

            write!(w, "        Ok(Self(")?;
            write_ty_ref(w, ty, true, ctx)?;
            writeln!(w, "::decode(r)?))")?;

            writeln!(w, "    }}")?;
            writeln!(w, "}}\n")?;
        }

        ir::TyKind::Enum(variants) if is_option_enum(variants) => {
            writeln!(w, "impl ::lutra_bin::Decode for {name} {{")?;
            writeln!(
                w,
                "    fn decode(r: &mut ::lutra_bin::Reader<'_>) -> ::lutra_bin::Result<Self> {{"
            )?;

            write!(w, "        Ok(Self(")?;
            write_ty_ref(w, ty, true, ctx)?;
            writeln!(w, "::decode(r)?))")?;

            writeln!(w, "    }}")?;
            writeln!(w, "}}\n")?;
        }

        ir::TyKind::Tuple(fields) => {
            writeln!(w, "impl ::lutra_bin::Decode for {name} {{")?;
            writeln!(w, "    fn decode(r: &mut ::lutra_bin::Reader<'_>) -> ::lutra_bin::Result<Self> {{")?;

            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);
                let field_ty = &field.ty;

                write!(w, "        let {field_name} = ")?;

                write_ty_ref(w, field_ty, true, ctx)?;

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

        ir::TyKind::Enum(variants) => {
            writeln!(w, "impl ::lutra_bin::Decode for {name} {{")?;
            writeln!(
                w,
                "    fn decode(r: &mut ::lutra_bin::Reader<'_>) -> ::lutra_bin::Result<Self> {{"
            )?;

            let head = layout::enum_head_format(variants);

            // tag
            writeln!(w, "        let mut tag_bytes = r.read_n({}).to_vec();", head.s / 8)?;
            writeln!(w, "        tag_bytes.resize(8, 0);")?;
            writeln!(w, "        let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;")?;

            writeln!(w, "        Ok(match tag {{")?;
            for (index, variant) in variants.iter().enumerate() {
                writeln!(w, "            {index} => {{")?;

                let variant_format = layout::enum_variant_format(&variant.ty);

                if variant_format.is_inline {
                    if !is_unit_variant(&variant.ty) {
                        write!(w, "                let inner = ")?;
                        write_ty_ref(w, &variant.ty, true, ctx)?;
                        writeln!(w, "::decode(r)?;")?;
                    }
                } else {
                    writeln!(w, "                let mut body = r.clone();")?;
                    writeln!(w, "                let offset = r.read_const::<4>();")?;
                    writeln!(w, "                let offset = u32::from_le_bytes(offset);")?;
                    writeln!(w, "                body.skip(offset as usize);")?;

                    write!(w, "                let inner = ")?;
                    write_ty_ref(w, &variant.ty, true, ctx)?;
                    writeln!(w, "::decode(&mut body)?;")?;
                }

                if variant_format.padding > 0 {
                    writeln!(w, "                r.skip({});", variant_format.padding / 8)?;
                }

                let needs_box = layout::does_enum_variant_contain_recursive(ty, index as u16);

                if is_unit_variant(&variant.ty) {
                    writeln!(w, "                {name}::{}", variant.name)?;
                } else if needs_box {
                    writeln!(w, "                {name}::{}(Box::new(inner))", variant.name)?;
                } else {
                    writeln!(w, "                {name}::{}(inner)", variant.name)?;
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

fn is_unit_variant(variant_ty: &ir::Ty) -> bool {
    variant_ty.kind.as_tuple().map_or(false, |f| f.is_empty())
}

fn is_option_enum(variants: &[ir::TyEnumVariant]) -> bool {
    variants.len() == 2 && is_unit_variant(&variants[0].ty) && !is_unit_variant(&variants[1].ty)
}
