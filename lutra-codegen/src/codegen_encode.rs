use std::fmt::Write;

use lutra_bin::{ir, layout};

use crate::Context;
use crate::codegen_ty::{
    is_option_enum, is_unit_variant, tuple_field_name, variant_needs_box, write_ty_ref,
};

pub fn write_encode_impls(
    w: &mut impl Write,
    tys: &[ir::Ty],
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    if tys.is_empty() {
        return Ok(());
    }

    writeln!(w, "mod impls {{")?;
    writeln!(w, "#![allow(unused_imports)]")?;
    writeln!(w, "use {}::ReaderExt;", ctx.options.lutra_bin_path)?;
    writeln!(w, "use {}::bytes::BufMut;", ctx.options.lutra_bin_path)?;
    writeln!(w, "use super::*;\n")?;

    ctx.current_rust_mod.push("impls".into());
    for ty in tys {
        write_ty_def_impl(w, ty, ctx)?;
    }
    ctx.current_rust_mod.pop();

    ctx.def_buffer.clear(); // all defs must have been already generated

    writeln!(w, "}}")
}

/// Generates the impl encode for a type.
#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
fn write_ty_def_impl(
    w: &mut impl Write,
    ty: &ir::Ty,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let lutra_bin = &ctx.options.lutra_bin_path;
    let name = ty.name.as_ref().unwrap();

    match &ty.kind {
        ir::TyKind::Primitive(_) => {
            writeln!(w, "impl {lutra_bin}::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = ();")?;
            writeln!(w, "    fn encode_head(&self, buf: &mut {lutra_bin}::bytes::BytesMut) {{")?;
            writeln!(w, "        self.0.encode_head(buf)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_body(&self, _: (), _: &mut {lutra_bin}::bytes::BytesMut) {{}}")?;
            writeln!(w, "}}")?;
        }

        ir::TyKind::Array(_) => {
            writeln!(w, "impl {lutra_bin}::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = {lutra_bin}::ReversePointer;")?;
            writeln!(w, "    fn encode_head(&self, buf: &mut {lutra_bin}::bytes::BytesMut) -> Self::HeadPtr {{")?;
            writeln!(w, "        self.0.encode_head(buf)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_body(&self, head: Self::HeadPtr, buf: &mut {lutra_bin}::bytes::BytesMut) {{")?;
            writeln!(w, "        self.0.encode_body(head, buf)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
        }

        ir::TyKind::Enum(variants) if is_option_enum(variants) => {
            let inner_ty = &variants[1].ty;

            let mut inner_head_ptr = String::new();
            write_ty_ref(&mut inner_head_ptr, inner_ty, true, ctx)?;

            writeln!(w, "impl {lutra_bin}::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = Option<Result<")?;
            writeln!(w, "         <{inner_head_ptr} as {lutra_bin}::Encode>::HeadPtr,")?;
            writeln!(w, "         {lutra_bin}::ReversePointer,")?;
            writeln!(w, "    >>;")?;
            writeln!(w, "    fn encode_head(&self, buf: &mut {lutra_bin}::bytes::BytesMut) -> Self::HeadPtr {{")?;
            writeln!(w, "        self.0.encode_head(buf)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_body(&self, head: Self::HeadPtr, buf: &mut {lutra_bin}::bytes::BytesMut) {{")?;
            writeln!(w, "        self.0.encode_body(head, buf)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
        }

        ir::TyKind::Tuple(fields) => {
            writeln!(w, "#[allow(clippy::all, unused_variables)]")?;
            writeln!(w, "impl {lutra_bin}::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = {name}HeadPtr;")?;

            // encode head
            writeln!(w, "    fn encode_head(&self, buf: &mut {lutra_bin}::bytes::BytesMut) -> Self::HeadPtr {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "        let {field_name} = self.{field_name}.encode_head(buf);")?;
            }
            writeln!(w, "        {name}HeadPtr {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "            {field_name},")?;
            }
            writeln!(w, "        }}")?;
            writeln!(w, "    }}")?;

            // encode body
            writeln!(w, "    fn encode_body(&self, head: Self::HeadPtr, buf: &mut {lutra_bin}::bytes::BytesMut) {{")?;

            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "        self.{field_name}.encode_body(head.{field_name}, buf);")?;
            }

            writeln!(w, "    }}")?;

            writeln!(w, "}}")?;

            // head ptr struct
            writeln!(w, "#[allow(non_camel_case_types)]")?;
            writeln!(w, "pub struct {name}HeadPtr {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                write!(w, "    {field_name}: <")?;

                write_ty_ref(w, &field.ty, true, ctx)?;

                writeln!(w, " as {lutra_bin}::Encode>::HeadPtr,")?;
            }
            writeln!(w, "}}")?;
        }

        ir::TyKind::Enum(variants) => {
            let head = layout::enum_head_format(variants);

            let needs_head_ptr = head.has_ptr;
            let head_ptr_name = if needs_head_ptr {
                format!("{name}HeadPtr")
            } else {
                "()".to_string()
            };

            writeln!(w, "#[allow(unused_variables)]")?;
            writeln!(w, "#[allow(clippy::all)]")?;
            writeln!(w, "impl {lutra_bin}::Encode for {name} {{")?;
            writeln!(w, "    type HeadPtr = {head_ptr_name};")?;
            writeln!(w, "    fn encode_head(&self, w: &mut {lutra_bin}::bytes::BytesMut) -> {head_ptr_name} {{")?;
            writeln!(w, "        match self {{")?;

            for (tag, variant) in variants.iter().enumerate() {
                let va_format = layout::enum_variant_format(&head, &variant.ty);

                write!(w, "            Self::{}", variant.name)?;

                if is_unit_variant(&variant.ty) {
                } else if head.inner_bytes == 0 {
                    write!(w, "(_)")?;
                } else {
                    write!(w, "(inner)")?;
                }
                writeln!(w, " => {{")?;

                let tag_bytes = &tag.to_le_bytes()[0..head.tag_bytes as usize];
                writeln!(w, "                w.put_slice(&{tag_bytes:?});")?;

                if needs_head_ptr {
                    if is_unit_variant(&variant.ty) {
                        writeln!(w, "                let r = {head_ptr_name}::None;")?;
                    } else if head.has_ptr {
                        writeln!(w, "                let head_ptr = {lutra_bin}::ReversePointer::new(w);")?;
                        writeln!(w, "                let r = {head_ptr_name}::{}(head_ptr);", variant.name)?;
                    } else {
                        writeln!(w, "                let inner_head_ptr = inner.encode_head(w);")?;
                        writeln!(w, "                let r = {head_ptr_name}::{}({lutra_bin}::boxed::Box::new(inner_head_ptr));", variant.name)?;
                    }
                } else if head.inner_bytes > 0 {
                    writeln!(w, "                inner.encode_head(w);")?;
                }

                if va_format.padding_bytes > 0 {
                    write!(w, "                w.put_bytes(0, {});", va_format.padding_bytes)?;
                }
                if needs_head_ptr {
                    writeln!(w, "                r")?;
                }

                writeln!(w, "            }},")?;
            }
            writeln!(w, "        }}")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_body(&self, head: {head_ptr_name}, w: &mut {lutra_bin}::bytes::BytesMut) {{")?;
            if needs_head_ptr {
                writeln!(w, "        match self {{")?;

                for variant in variants {
                    write!(w, "            Self::{}", variant.name)?;
                    if !is_unit_variant(&variant.ty) {
                        write!(w, "(inner)")?;
                    }
                    writeln!(w, " => {{")?;

                    if is_unit_variant(&variant.ty) {
                        // unit does not have a body
                    } else if head.has_ptr {
                        writeln!(w, "                let {head_ptr_name}::{}(offset_ptr) = head else {{ unreachable!() }};", variant.name)?;
                        writeln!(w, "                offset_ptr.write_cur_len(w);")?;
                        writeln!(w, "                let inner_head_ptr = inner.encode_head(w);")?;
                        writeln!(w, "                inner.encode_body(inner_head_ptr, w);")?;
                    } else {
                        writeln!(w, "                let {head_ptr_name}::{}(inner_head_ptr) = head else {{ unreachable!() }};", variant.name)?;
                        writeln!(w, "                inner.encode_body(*inner_head_ptr, w);")?;
                    }

                    writeln!(w, "            }},")?;
                }

                writeln!(w, "        }}")?;
            }
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

                    write!(w, "    {}", variant.name)?;

                    if head.has_ptr {
                        write!(w, "({lutra_bin}::ReversePointer)")?;
                    } else {
                        write!(w, "({lutra_bin}::boxed::Box<<")?;
                        write_ty_ref(w, &variant.ty, false, ctx)?;
                        write!(w, " as {lutra_bin}::Encode>::HeadPtr>)")?;
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
        .unwrap_or_else(|| panic!("ty is missing layout: {}", lutra_bin::ir::print_ty(ty)))
        .head_size;
    writeln!(w, "impl {lutra_bin}::Layout for {name} {{")?;
    writeln!(w, "    fn head_size() -> usize {{")?;
    writeln!(w, "        {head_size}")?;
    writeln!(w, "    }}")?;
    writeln!(w, "}}\n")?;

    match &ty.kind {
        ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
            writeln!(w, "impl {lutra_bin}::Decode for {name} {{")?;
            writeln!(
                w,
                "    fn decode(buf: &[u8]) -> {lutra_bin}::Result<Self> {{"
            )?;

            write!(w, "        Ok(Self(")?;
            write_ty_ref(w, ty, true, ctx)?;
            writeln!(w, "::decode(buf)?))")?;

            writeln!(w, "    }}")?;
            writeln!(w, "}}\n")?;
        }

        ir::TyKind::Enum(variants) if is_option_enum(variants) => {
            writeln!(w, "impl {lutra_bin}::Decode for {name} {{")?;
            writeln!(
                w,
                "    fn decode(buf: &[u8]) -> {lutra_bin}::Result<Self> {{"
            )?;

            write!(w, "        Ok(Self(")?;
            write_ty_ref(w, ty, true, ctx)?;
            writeln!(w, "::decode(buf)?))")?;

            writeln!(w, "    }}")?;
            writeln!(w, "}}\n")?;
        }

        ir::TyKind::Tuple(fields) => {
            if fields.is_empty() {
                writeln!(w, "#[allow(unused_variables)]")?;
            }

            writeln!(w, "impl {lutra_bin}::Decode for {name} {{")?;
            writeln!(w, "    fn decode(buf: &[u8]) -> {lutra_bin}::Result<Self> {{")?;

            let field_offsets = layout::tuple_field_offsets(ty);
            for (index, (field, offset)) in fields.iter().zip(field_offsets).enumerate() {
                let field_name = tuple_field_name(&field.name, index);
                let field_ty = &field.ty;

                write!(w, "        let {field_name} = ")?;

                write_ty_ref(w, field_ty, true, ctx)?;

                writeln!(w, "::decode(buf.skip({offset}))?;")?;
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
            writeln!(w, "impl {lutra_bin}::Decode for {name} {{")?;
            writeln!(
                w,
                "    fn decode(buf: &[u8]) -> {lutra_bin}::Result<Self> {{"
            )?;

            let head = layout::enum_head_format(variants);

            // tag
            writeln!(w, "        let mut tag_bytes = buf.read_n({}).to_vec();", head.tag_bytes)?;
            writeln!(w, "        tag_bytes.resize(8, 0);")?;
            writeln!(w, "        let tag = u64::from_le_bytes(tag_bytes.try_into().unwrap()) as usize;")?;

            if variants.iter().any(|v| !is_unit_variant(&v.ty)) {
                writeln!(w, "        let buf = buf.skip({});", head.tag_bytes)?;
            }

            writeln!(w, "        Ok(match tag {{")?;
            for (index, variant) in variants.iter().enumerate() {
                writeln!(w, "            {index} => {{")?;

                if is_unit_variant(&variant.ty) {
                } else if head.has_ptr {
                    writeln!(w, "                let offset = u32::from_le_bytes(buf.read_const::<4>());")?;

                    write!(w, "                let inner = ")?;
                    write_ty_ref(w, &variant.ty, true, ctx)?;
                    writeln!(w, "::decode(buf.skip(offset as usize))?;")?;
                } else {
                    write!(w, "                let inner = ")?;
                    write_ty_ref(w, &variant.ty, true, ctx)?;
                    writeln!(w, "::decode(buf)?;")?;
                }

                let needs_box = variant_needs_box(ty, index);

                if is_unit_variant(&variant.ty) {
                    writeln!(w, "                {name}::{}", variant.name)?;
                } else if needs_box {
                    writeln!(w, "                {name}::{}({lutra_bin}::boxed::Box::new(inner))", variant.name)?;
                } else {
                    writeln!(w, "                {name}::{}(inner)", variant.name)?;
                }

                writeln!(w, "            }},")?;
            }
            writeln!(w, "            _ => return Err({lutra_bin}::Error::InvalidData)")?;
            writeln!(w, "        }})")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}\n")?;
        }

        _ => unimplemented!(),
    }

    Ok(())
}
