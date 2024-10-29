use std::ops::Mul;

use lutra_parser::parser::pr;

use crate::{Error, Result};

pub trait Layout {
    /// Returns the size of the head in bits for a given type.
    fn head_size() -> usize;
}

impl Layout for bool {
    fn head_size() -> usize {
        8
    }
}

impl Layout for i64 {
    fn head_size() -> usize {
        64
    }
}

impl Layout for f64 {
    fn head_size() -> usize {
        64
    }
}

impl Layout for String {
    fn head_size() -> usize {
        64
    }
}

impl<I> Layout for Vec<I> {
    fn head_size() -> usize {
        64
    }
}

pub fn get_head_size(ty: &pr::Ty) -> Result<usize> {
    Ok(match &ty.kind {
        pr::TyKind::Primitive(pr::PrimitiveSet::Bool) => 8,
        pr::TyKind::Primitive(pr::PrimitiveSet::Int) => 64,
        pr::TyKind::Primitive(pr::PrimitiveSet::Float) => 64,
        pr::TyKind::Primitive(pr::PrimitiveSet::Text) => 64,
        pr::TyKind::Array(_) => 64,

        pr::TyKind::Tuple(fields) => {
            let mut size = 0;
            for f in fields {
                size += get_head_size(&f.ty)?;
            }
            size
        }
        pr::TyKind::Enum(variants) => {
            let head = enum_head_format(variants)?;
            if head.is_always_inline {
                head.s + head.h
            } else {
                head.s + 32
            }
        }

        pr::TyKind::Primitive(_) | pr::TyKind::Function(_) | pr::TyKind::Ident(_) => {
            return Err(Error::InvalidType)
        }
    })
}

pub struct EnumHeadFormat {
    pub s: usize,
    pub h: usize,
    pub is_always_inline: bool,
}

pub fn enum_head_format(variants: &[(String, pr::Ty)]) -> Result<EnumHeadFormat> {
    let s = enum_tag_size(variants.len());

    let h = enum_max_variant_head_size(variants)?;

    Ok(EnumHeadFormat {
        s,
        h,
        is_always_inline: s + h <= 64,
    })
}

pub struct EnumVariantFormat {
    pub padding: usize,
    pub is_inline: bool,
}

pub fn enum_variant_format(
    head: &EnumHeadFormat,
    variant_ty: &pr::Ty,
) -> Result<EnumVariantFormat> {
    let variant_size = get_head_size(variant_ty)?;

    Ok(if head.is_always_inline {
        EnumVariantFormat {
            is_inline: true,
            padding: head.h - variant_size,
        }
    } else {
        let is_inline = head.s + variant_size <= 32;
        let padding = 32_usize.saturating_sub(variant_size);

        EnumVariantFormat { is_inline, padding }
    })
}

fn enum_max_variant_head_size(variants: &[(String, pr::Ty)]) -> Result<usize> {
    let mut h = 0;
    for (_n, ty) in variants {
        let size = get_head_size(ty)?;
        h = h.max(size);
    }
    Ok(h)
}

fn enum_tag_size(variants_len: usize) -> usize {
    enum_tag_size_used(variants_len).div_ceil(8).mul(8)
}

fn enum_tag_size_used(variants_len: usize) -> usize {
    f64::log2(variants_len as f64).ceil() as usize
}

#[test]
fn test_enum_tag_size() {
    assert_eq!(0, enum_tag_size_used(0));
    assert_eq!(0, enum_tag_size_used(1));
    assert_eq!(1, enum_tag_size_used(2));
    assert_eq!(2, enum_tag_size_used(3));
    assert_eq!(2, enum_tag_size_used(4));
    assert_eq!(3, enum_tag_size_used(5));
    assert_eq!(3, enum_tag_size_used(6));
    assert_eq!(3, enum_tag_size_used(7));
    assert_eq!(3, enum_tag_size_used(8));
    assert_eq!(4, enum_tag_size_used(9));
    assert_eq!(4, enum_tag_size_used(10));
    assert_eq!(4, enum_tag_size_used(11));
    assert_eq!(4, enum_tag_size_used(12));
    assert_eq!(4, enum_tag_size_used(13));
    assert_eq!(4, enum_tag_size_used(14));
    assert_eq!(4, enum_tag_size_used(15));
    assert_eq!(4, enum_tag_size_used(16));
    assert_eq!(5, enum_tag_size_used(17));
    assert_eq!(5, enum_tag_size_used(18));
    assert_eq!(5, enum_tag_size_used(19));
    assert_eq!(5, enum_tag_size_used(20));
    assert_eq!(5, enum_tag_size_used(21));
    assert_eq!(5, enum_tag_size_used(22));
}
