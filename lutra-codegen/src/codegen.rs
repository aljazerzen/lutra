use std::{borrow::Cow, fmt::Write};

use lutra_bin::layout;
use lutra_frontend::decl;
use lutra_frontend::pr;

pub fn codegen(source: &str) -> Result<String, std::fmt::Error> {
    let source = lutra_frontend::SourceTree::single("".into(), source.into());
    let project = lutra_frontend::compile(source, lutra_frontend::CompileParams {}).unwrap();

    let mut w = String::new();
    writeln!(w, "use std::io::Write;\n")?;

    let mut ctx = Context::default();

    for path in &project.root_module.ordering {
        let mut decl = project.root_module.module.get(path).unwrap().clone();

        match &mut decl.kind {
            decl::DeclKind::Ty(ty) => {
                infer_names(path.name(), ty);

                // run layout ahead of time to resolve recursive references
                layout::get_head_size(ty, &mut ctx.cache).unwrap();

                write_ty_def(&mut w, ty, &mut ctx)?;
            }
            _ => {}
        }

        while !ctx.def_buffer.is_empty() {
            let ty = ctx.def_buffer.remove(0);
            write_ty_def(&mut w, &ty, &mut ctx)?;
        }
    }
    Ok(w)
}

/// Types might not have names, because they are defined inline.
/// This function traverses a type definition and generates names for all of the types.
fn infer_names(stmt_name: &str, ty: &mut pr::Ty) {
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
struct Context {
    /// Buffer for types that need their definitions generated.
    def_buffer: Vec<pr::Ty>,

    cache: layout::LayoutCache,
}

/// Generates a type definition.
fn write_ty_def<'t>(
    w: &mut impl Write,
    ty: &'t pr::Ty,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let name = ty.name.as_ref().unwrap();

    writeln!(w, "#[derive(Debug, Clone)]")?;
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
                    let needs_box = ctx.cache.does_enum_variant_contain_recursive(ty, index);

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

    write_ty_def_impl(w, ty, ctx)?;

    Ok(())
}

fn tuple_field_name(name: &Option<String>, index: usize) -> Cow<'_, str> {
    (name.as_ref())
        .map(|x| Cow::Borrowed(x.as_str()))
        .unwrap_or_else(|| format!("field{index}").into())
}

/// Generates a reference to a type.
/// Syntactically, this could be used in `let x: type_ref`.
fn write_ty_ref<'t>(
    w: &mut impl Write,
    ty: &'t pr::Ty,
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
            ctx.def_buffer.push(ty.clone());

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
fn write_ty_def_impl<'t>(
    w: &mut impl Write,
    ty: &'t pr::Ty,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let name = ty.name.as_ref().unwrap();

    match &ty.kind {
        pr::TyKind::Primitive(_) => {
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type BodyMeta = ();")?;
            writeln!(w, "    fn encode_body(&self, _w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            writeln!(w, "        Ok(())")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_head(&self, _: (), w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            writeln!(w, "        self.0.encode_head((), w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
        }

        pr::TyKind::Array(_) => {
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type BodyMeta = usize;")?;
            writeln!(w, "    fn encode_body(&self, w: &mut Vec<u8>) -> ::lutra_bin::Result<usize> {{")?;
            writeln!(w, "        self.0.encode_body(w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_head(&self, meta: usize, w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            writeln!(w, "        self.0.encode_head(meta, w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
        }

        pr::TyKind::Tuple(fields) => {
            writeln!(w, "#[allow(clippy::all)]")?;
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type BodyMeta = {name}BodyMeta;")?;

            // encode body
            writeln!(w, "    fn encode_body(&self, w: &mut Vec<u8>) -> ::lutra_bin::Result<Self::BodyMeta> {{")?;

            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "        let {0} = self.{0}.encode_body(w)?;", field_name)?;
            }

            writeln!(w, "        Ok({name}BodyMeta {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "            {field_name},")?;
            }
            writeln!(w, "        }})")?;
            writeln!(w, "    }}")?;

            // encode head
            writeln!(w, "    fn encode_head(&self, meta: Self::BodyMeta, w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "        self.{0}.encode_head(meta.{0}, w)?;", field_name)?;
            }
            writeln!(w, "        Ok(())")?;
            writeln!(w, "    }}")?;

            writeln!(w, "}}")?;

            // body meta struct
            writeln!(w, "#[allow(non_camel_case_types)]")?;
            writeln!(w, "pub struct {name}BodyMeta {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                write!(w, "    {field_name}: <")?;

                let mut ctx = Context::default();
                write_ty_ref(w, &field.ty, true, &mut ctx)?;

                writeln!(w, " as ::lutra_bin::Encode>::BodyMeta,")?;
            }
            writeln!(w, "}}")?;
        }

        pr::TyKind::Enum(variants) => {
            let head = layout::enum_head_format(variants, &mut ctx.cache).unwrap();

            let needs_body_meta = variants.iter().any(|(_, t)| !is_unit_variant(t));
            let body_meta_name = if needs_body_meta {
                format!("{name}BodyMeta")
            } else {
                "()".to_string()
            };

            writeln!(w, "#[allow(unused_variables)]")?;
            writeln!(w, "#[allow(clippy::all)]")?;
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type BodyMeta = {body_meta_name};")?;
            writeln!(w, "    fn encode_body(&self, w: &mut Vec<u8>) -> ::lutra_bin::Result<{body_meta_name}> {{")?;
            if needs_body_meta {
                writeln!(w, "        Ok(match self {{")?;

                for (variant_name, variant_ty) in variants {
                    write!(w, "            Self::{variant_name}")?;
                    if !is_unit_variant(variant_ty) {
                        write!(w, "(inner)")?;
                    }
                    writeln!(w, " => {{")?;

                    let variant =
                        layout::enum_variant_format(&head, variant_ty, &mut ctx.cache).unwrap();

                    if is_unit_variant(variant_ty) {
                        writeln!(w, "                {body_meta_name}::None")?;
                    } else if variant.is_inline {
                        writeln!(w, "                let meta = inner.encode_body(w)?;")?;
                        writeln!(w, "                {body_meta_name}::{variant_name}(meta)")?;
                    } else {
                        writeln!(w, "                let meta = inner.encode_body(w)?;")?;
                        writeln!(w, "                let start = w.len();")?;
                        writeln!(w, "                inner.encode_head(meta, w)?;")?;
                        writeln!(w, "                {body_meta_name}::{variant_name}(start)")?;
                    }

                    writeln!(w, "            }},")?;
                }

                writeln!(w, "        }})")?;
            } else {
                writeln!(w, "        Ok(())")?;
            }
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_head(&self, meta: {body_meta_name}, w: &mut Vec<u8>) -> ::lutra_bin::Result<()> {{")?;
            writeln!(w, "        Ok(match self {{")?;

            for (tag, (variant_name, variant_ty)) in variants.iter().enumerate() {
                let variant =
                    layout::enum_variant_format(&head, variant_ty, &mut ctx.cache).unwrap();

                write!(w, "            Self::{variant_name}")?;

                if !variant.is_inline {
                    write!(w, "(_)")?;
                } else if !is_unit_variant(variant_ty) {
                    write!(w, "(inner)")?;
                }
                writeln!(w, " => {{")?;

                if !variant.is_inline {
                    writeln!(w, "                let {body_meta_name}::{variant_name}(meta) = meta else {{ unreachable!() }};")?;
                    writeln!(w, "                let offset = (w.len() - meta) as u32;")?;
                }

                let tag_bytes = &tag.to_le_bytes()[0..head.s / 8];
                writeln!(w, "                w.write_all(&{tag_bytes:?})?;")?;

                if variant.is_inline {
                    if !is_unit_variant(variant_ty) {
                        writeln!(w, "                let {body_meta_name}::{variant_name}(meta) = meta else {{ unreachable!() }};")?;
                        writeln!(w, "                inner.encode_head(meta, w)?;")?;
                    }
                } else {
                    writeln!(w, "                w.write_all(&offset.to_le_bytes())?;")?;
                }

                if variant.padding > 0 {
                    write!(w, "                w.write_all(&[")?;
                    for _ in 0..(variant.padding / 8) {
                        write!(w, "0u8,")?;
                    }
                    writeln!(w, "])?;")?;
                }

                writeln!(w, "            }},")?;
            }

            writeln!(w, "        }})")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;

            if needs_body_meta {
                writeln!(w, "#[allow(non_camel_case_types, dead_code)]")?;
                writeln!(w, "pub enum {name}BodyMeta {{")?;
                writeln!(w, "    None,")?;
                for (variant_name, variant_ty) in variants {
                    if is_unit_variant(variant_ty) {
                        continue;
                    }

                    let variant =
                        layout::enum_variant_format(&head, variant_ty, &mut ctx.cache).unwrap();

                    write!(w, "    {variant_name}")?;

                    if variant.is_inline {
                        write!(w, "(<")?;
                        let mut ctx = Context::default();
                        write_ty_ref(w, variant_ty, false, &mut ctx)?;
                        write!(w, " as ::lutra_bin::Encode>::BodyMeta)")?;
                    } else {
                        write!(w, "(usize)")?;
                    }

                    writeln!(w, ",")?;
                }
                writeln!(w, "}}")?;
            }
        }

        _ => unimplemented!(),
    }

    let head_size = layout::get_head_size(ty, &mut ctx.cache).unwrap();
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

            let head = layout::enum_head_format(variants, &mut ctx.cache).unwrap();
            if !head.is_always_inline {
                writeln!(w, "        let mut body = r.clone();")?;
            }

            // tag
            writeln!(w, "        let mut tag_bytes = r.copy_n({});", head.s / 8)?;
            writeln!(w, "        tag_bytes.resize(8, 0);")?;
            writeln!(w, "        let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;")?;

            writeln!(w, "        Ok(match tag {{")?;
            for (index, (variant_name, variant_ty)) in variants.iter().enumerate() {
                writeln!(w, "            {index} => {{")?;

                let variant_format =
                    layout::enum_variant_format(&head, variant_ty, &mut ctx.cache).unwrap();

                if variant_format.is_inline {
                    if !is_unit_variant(variant_ty) {
                        write!(w, "                let inner = ")?;
                        let mut ctx = Context::default();
                        write_ty_ref(w, variant_ty, true, &mut ctx)?;
                        writeln!(w, "::decode(r)?;")?;
                    }
                } else {
                    writeln!(w, "                let offset = r.copy_const::<4>();")?;
                    writeln!(w, "                let offset = u32::from_le_bytes(offset);")?;
                    writeln!(w, "                body.rewind(offset as usize);")?;

                    write!(w, "                let inner = ")?;
                    let mut ctx = Context::default();
                    write_ty_ref(w, variant_ty, true, &mut ctx)?;
                    writeln!(w, "::decode(&mut body)?;")?;
                }

                if variant_format.padding > 0 {
                    writeln!(w, "                r.skip({});", variant_format.padding / 8)?;
                }

                let needs_box = ctx.cache.does_enum_variant_contain_recursive(ty, index);

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
