use crate::string;
use crate::vec;

use crate::ir;

pub trait Layout {
    /// Returns the size of the head in bits for a given type.
    fn head_size() -> usize;
}

impl Layout for bool {
    fn head_size() -> usize {
        8
    }
}

impl Layout for i8 {
    fn head_size() -> usize {
        8
    }
}
impl Layout for i16 {
    fn head_size() -> usize {
        16
    }
}
impl Layout for i32 {
    fn head_size() -> usize {
        32
    }
}
impl Layout for i64 {
    fn head_size() -> usize {
        64
    }
}
impl Layout for u8 {
    fn head_size() -> usize {
        8
    }
}
impl Layout for u16 {
    fn head_size() -> usize {
        16
    }
}
impl Layout for u32 {
    fn head_size() -> usize {
        32
    }
}
impl Layout for u64 {
    fn head_size() -> usize {
        64
    }
}
impl Layout for f32 {
    fn head_size() -> usize {
        32
    }
}
impl Layout for f64 {
    fn head_size() -> usize {
        64
    }
}

impl Layout for str {
    fn head_size() -> usize {
        64
    }
}
impl Layout for string::String {
    fn head_size() -> usize {
        64
    }
}

impl<I> Layout for vec::Vec<I> {
    fn head_size() -> usize {
        64
    }
}

impl<I: Layout> Layout for Option<I> {
    fn head_size() -> usize {
        if I::head_size() == 0 {
            8
        } else {
            40
        }
    }
}

// Keep in sync with [pr::TyKind::get_layout_simple]
// Does not recurse.
pub fn get_layout_simple(ty: &ir::Ty) -> Option<ir::TyLayout> {
    let head_size = match &ty.kind {
        ir::TyKind::Primitive(prim) => match prim {
            ir::TyPrimitive::int8 => 8,
            ir::TyPrimitive::int16 => 16,
            ir::TyPrimitive::int32 => 32,
            ir::TyPrimitive::int64 => 64,
            ir::TyPrimitive::uint8 => 8,
            ir::TyPrimitive::uint16 => 16,
            ir::TyPrimitive::uint32 => 32,
            ir::TyPrimitive::uint64 => 64,
            ir::TyPrimitive::float32 => 32,
            ir::TyPrimitive::float64 => 64,
            ir::TyPrimitive::bool => 8,
            ir::TyPrimitive::text => 64,
        },
        ir::TyKind::Array(_) => 64,

        ir::TyKind::Tuple(fields) => {
            let mut size = 0;
            for f in fields {
                if let Some(layout) = &f.ty.layout {
                    size += layout.head_size;
                } else {
                    return None;
                }
            }
            size
        }
        ir::TyKind::Enum(variants) => {
            let head = enum_head_format(variants);

            (head.tag_bytes + head.inner_bytes) * 8
        }
        _ => return None,
    };
    let body_ptrs: vec::Vec<u32> = match &ty.kind {
        ir::TyKind::Primitive(ir::TyPrimitive::text) => vec![0],
        ir::TyKind::Primitive(_) => vec![],

        ir::TyKind::Array(_) => vec![0],
        ir::TyKind::Enum(_variants) => {
            vec![]
            // let head = enum_head_format(variants);
            // if head.inner_bytes < 4 {
            //     vec![]
            // } else {
            //     // TODO: this is not true for all variants
            //     // There might be one variant that is inline
            //     // and one that is not.
            //     // TyLayout will have to be expaned to accomodate this.
            //     vec![head.tag_bytes]
            // }
        }

        ir::TyKind::Tuple(fields) => {
            let mut r = vec::Vec::new();
            let mut field_start = 0;
            for f in fields {
                let layout = f.ty.layout.as_ref().unwrap();
                r.extend(layout.body_ptrs.iter().map(|p| p + field_start));
                field_start += layout.head_size.div_ceil(8);
            }
            r
        }

        _ => return None,
    };
    Some(ir::TyLayout {
        head_size,
        body_ptrs,
    })
}

pub fn tuple_field_offsets(ty: &ir::Ty) -> vec::Vec<u32> {
    let ir::TyKind::Tuple(ty_fields) = &ty.kind else {
        panic!("got: {:?}", ty.kind)
    };

    let mut field_offsets = vec::Vec::with_capacity(ty_fields.len());
    let mut offset = 0_u32;
    for field in ty_fields {
        field_offsets.push(offset);

        let layout = field.ty.layout.as_ref().unwrap();
        offset += (layout.head_size).div_ceil(8);
    }
    field_offsets
}

pub fn tuple_field_offset(ty: &ir::Ty, position: u16) -> u32 {
    *tuple_field_offsets(ty).get(position as usize).unwrap()
}

pub fn does_enum_variant_contain_recursive(enum_ty: &ir::Ty, variant_index: u16) -> bool {
    enum_ty.variants_recursive.contains(&variant_index)
}

#[derive(Debug)]
pub struct EnumHeadFormat {
    pub tag_bytes: u32,
    pub inner_bytes: u32,
    pub has_ptr: bool,
}

pub fn enum_head_format(variants: &[ir::TyEnumVariant]) -> EnumHeadFormat {
    let t = enum_tag_size(variants.len());

    let max_head = enum_max_variant_head_size(variants);

    let mut has_ptr = false;
    let inner = if max_head > 32 {
        // insert a pointer
        has_ptr = true;
        32
    } else {
        max_head
    };

    EnumHeadFormat {
        tag_bytes: t.div_ceil(8),
        inner_bytes: inner.div_ceil(8),
        has_ptr,
    }
}

#[derive(Debug)]
pub struct EnumVariantFormat {
    pub is_unit: bool,
    pub padding_bytes: u32,
}

pub fn enum_variant_format(head: &EnumHeadFormat, variant_ty: &ir::Ty) -> EnumVariantFormat {
    let inner_head_size = variant_ty.layout.as_ref().unwrap().head_size;
    let inner_head_bytes = inner_head_size.div_ceil(8);

    let is_unit = inner_head_bytes == 0;

    let padding_bytes = if head.has_ptr {
        if is_unit {
            // unit variant does not have a pointer, it is all padding
            4
        } else {
            0
        }
    } else {
        head.inner_bytes.saturating_sub(inner_head_bytes)
    };

    EnumVariantFormat {
        padding_bytes,
        is_unit,
    }
}

fn enum_max_variant_head_size(variants: &[ir::TyEnumVariant]) -> u32 {
    let mut i = 0;
    for variant in variants {
        let ty_layout = variant
            .ty
            .layout
            .as_ref()
            .unwrap_or_else(|| panic!("missing layout: {:?}", variant.ty));
        let size = ty_layout.head_size;
        i = i.max(size);
    }
    i
}

fn enum_tag_size(variants_len: usize) -> u32 {
    // TODO: when bool-sub-byte packing is implemented, remove function in favor of enum_tag_size_used
    enum_tag_size_used(variants_len).div_ceil(8) * 8
}

fn enum_tag_size_used(variants_len: usize) -> u32 {
    variants_len
        .saturating_sub(1)
        .checked_ilog2()
        .map(|x| x + 1)
        .unwrap_or_default()
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
    assert_eq!(8, enum_tag_size_used(256));
    assert_eq!(9, enum_tag_size_used(257));

    assert_eq!(0, enum_tag_size(0));
    assert_eq!(0, enum_tag_size(1));
    assert_eq!(8, enum_tag_size(2));
    assert_eq!(8, enum_tag_size(3));
    assert_eq!(8, enum_tag_size(4));
    assert_eq!(8, enum_tag_size(5));
    assert_eq!(8, enum_tag_size(6));
    assert_eq!(8, enum_tag_size(7));
    assert_eq!(8, enum_tag_size(8));
    assert_eq!(8, enum_tag_size(9));
    assert_eq!(8, enum_tag_size(10));
    assert_eq!(8, enum_tag_size(11));
    assert_eq!(8, enum_tag_size(12));
    assert_eq!(8, enum_tag_size(13));
    assert_eq!(8, enum_tag_size(14));
    assert_eq!(8, enum_tag_size(15));
    assert_eq!(8, enum_tag_size(16));
    assert_eq!(8, enum_tag_size(17));
    assert_eq!(8, enum_tag_size(18));
    assert_eq!(8, enum_tag_size(19));
    assert_eq!(8, enum_tag_size(20));
    assert_eq!(8, enum_tag_size(21));
    assert_eq!(8, enum_tag_size(22));
    assert_eq!(8, enum_tag_size(256));
    assert_eq!(16, enum_tag_size(257));
}
