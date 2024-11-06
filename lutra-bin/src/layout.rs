use std::ops::Mul;

use lutra_frontend::pr;

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
pub struct LayoutCache {}

impl LayoutCache {
    pub fn does_enum_variant_contain_recursive(
        &self,
        enum_ty: &pr::Ty,
        variant_index: usize,
    ) -> bool {
        let layout = enum_ty.layout.as_ref().unwrap();
        layout.variants_recursive.contains(&variant_index)
    }
}

pub struct EnumHeadFormat {
    pub s: usize,
    pub h: usize,
    pub no_pointer: bool,
}

pub fn enum_head_format(variants: &[(String, pr::Ty)]) -> EnumHeadFormat {
    let s = enum_tag_size(variants.len());

    let h = enum_max_variant_head_size(variants);

    EnumHeadFormat {
        s,
        h: if h == 0 { 0 } else { 32 },
        no_pointer: h == 0,
    }
}
pub struct EnumVariantFormat {
    pub padding: usize,
    pub is_inline: bool,
}

pub fn enum_variant_format(variant_ty: &pr::Ty) -> EnumVariantFormat {
    let variant_size = variant_ty.layout.as_ref().unwrap().head_size;

    let is_inline = variant_size <= 32;
    let padding = 32_usize.saturating_sub(variant_size);

    EnumVariantFormat { is_inline, padding }
}

fn enum_max_variant_head_size(variants: &[(String, pr::Ty)]) -> usize {
    let mut h = 0;
    for (_n, ty) in variants {
        let size = ty.layout.as_ref().unwrap().head_size;
        h = h.max(size);
    }
    h
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
