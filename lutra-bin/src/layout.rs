use std::{collections::HashMap, ops::Mul};

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

#[derive(Default)]
pub struct LayoutCache {
    head_size_cache: HashMap<String, usize>,
}

impl LayoutCache {
    // fn contains_name(&self, name: &str) -> bool {
    //     self.head_size_cache.contains_key(name)
    // }
}

pub fn get_head_size(ty: &pr::Ty, cache: &mut LayoutCache) -> Result<usize> {
    if let Some(name) = &ty.name {
        if let Some(existing) = cache.head_size_cache.get(name) {
            return Ok(*existing);
        }
    }

    let size = match &ty.kind {
        pr::TyKind::Primitive(pr::PrimitiveSet::Bool) => 8,
        pr::TyKind::Primitive(pr::PrimitiveSet::Int) => 64,
        pr::TyKind::Primitive(pr::PrimitiveSet::Float) => 64,
        pr::TyKind::Primitive(pr::PrimitiveSet::Text) => 64,
        pr::TyKind::Array(_) => 64,

        pr::TyKind::Tuple(fields) => {
            let mut size = 0;
            for f in fields {
                size += get_head_size(&f.ty, cache)?;
            }
            size
        }
        pr::TyKind::Enum(variants) => {
            let head = enum_head_format(variants, cache)?;
            if head.is_always_inline {
                head.s + head.h
            } else {
                head.s + 32
            }
        }

        pr::TyKind::Ident(ident) => {
            if let Some(val) = cache.head_size_cache.get(&ident.name) {
                *val
            } else {
                // if target of the ident has not yet been resolved, the type is invalid
                return Err(Error::InvalidType);
            }
        }

        pr::TyKind::Primitive(_) | pr::TyKind::Function(_) => return Err(Error::InvalidType),
    };

    if let Some(name) = &ty.name {
        cache.head_size_cache.insert(name.clone(), size);
    }
    Ok(size)
}

pub struct EnumHeadFormat {
    pub s: usize,
    pub h: usize,
    pub is_always_inline: bool,
}

pub fn enum_head_format(
    variants: &[(String, pr::Ty)],
    cache: &mut LayoutCache,
) -> Result<EnumHeadFormat> {
    let s = enum_tag_size(variants.len());

    let h = enum_max_variant_head_size(variants, cache)?;

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
    cache: &mut LayoutCache,
) -> Result<EnumVariantFormat> {
    let variant_size = get_head_size(variant_ty, cache)?;

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

fn enum_max_variant_head_size(
    variants: &[(String, pr::Ty)],
    cache: &mut LayoutCache,
) -> Result<usize> {
    let mut h = 0;
    for (_n, ty) in variants {
        let size = get_head_size(ty, cache)?;
        h = h.max(size);
    }
    Ok(h)
}

fn enum_tag_size(variants_len: usize) -> usize {
    // TODO: when bool-sub-byte packing is implemented, remove function in favor of enum_tag_size_used
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
