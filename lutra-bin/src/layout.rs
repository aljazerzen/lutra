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

impl<I: Layout> Layout for Option<I> {
    fn head_size() -> usize {
        if I::head_size() == 0 {
            8
        } else {
            40
        }
    }
}

pub fn get_layout_simple(ty: &ir::Ty) -> Option<ir::TyLayout> {
    let head_size = match &ty.kind {
        ir::TyKind::Primitive(prim) => match prim {
            ir::PrimitiveSet::int8 => 8,
            ir::PrimitiveSet::int16 => 16,
            ir::PrimitiveSet::int32 => 32,
            ir::PrimitiveSet::int64 => 64,
            ir::PrimitiveSet::uint8 => 8,
            ir::PrimitiveSet::uint16 => 16,
            ir::PrimitiveSet::uint32 => 32,
            ir::PrimitiveSet::uint64 => 64,
            ir::PrimitiveSet::float32 => 32,
            ir::PrimitiveSet::float64 => 64,
            ir::PrimitiveSet::bool => 8,
            ir::PrimitiveSet::text => 64,
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
            let tag_size = enum_tag_size(variants.len());

            tag_size + 32
        }
        _ => return None,
    };
    let body_ptr_offset: Vec<u32> = match &ty.kind {
        ir::TyKind::Primitive(ir::PrimitiveSet::text) => vec![0],
        ir::TyKind::Primitive(_) => vec![],

        ir::TyKind::Array(_) => vec![0],
        ir::TyKind::Enum(_) => vec![1], // TODO: this is wrong (in some cases)

        ir::TyKind::Tuple(fields) => {
            let mut r = Vec::new();
            for f in fields {
                let ty = f.ty.layout.as_ref().unwrap();
                r.extend(&ty.body_ptrs);
            }
            r
        }

        _ => return None,
    };
    Some(ir::TyLayout {
        head_size,
        body_ptrs: body_ptr_offset,
        variants_recursive: vec![],
    })
}

pub fn tuple_field_offsets(ty: &ir::Ty) -> Vec<u32> {
    let ir::TyKind::Tuple(ty_fields) = &ty.kind else {
        panic!()
    };

    let mut field_offsets = Vec::with_capacity(ty_fields.len());
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
    let layout = enum_ty.layout.as_ref().unwrap();
    layout.variants_recursive.contains(&variant_index)
}

pub struct EnumHeadFormat {
    pub s: usize,
    pub h: usize,
    pub no_pointer: bool,
}

pub fn enum_head_format(variants: &[ir::TyEnumVariant]) -> EnumHeadFormat {
    let s = enum_tag_size(variants.len()) as usize;

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

pub fn enum_variant_format(variant_ty: &ir::Ty) -> EnumVariantFormat {
    let variant_size = variant_ty.layout.as_ref().unwrap().head_size as usize;

    let is_inline = variant_size <= 32;
    let padding = 32_usize.saturating_sub(variant_size);

    EnumVariantFormat { is_inline, padding }
}

fn enum_max_variant_head_size(variants: &[ir::TyEnumVariant]) -> usize {
    let mut h = 0;
    for variant in variants {
        let ty_layout = variant
            .ty
            .layout
            .as_ref()
            .unwrap_or_else(|| panic!("missing layout: {:?}", variant.ty));
        let size = ty_layout.head_size as usize;
        h = h.max(size);
    }
    h
}

fn enum_tag_size(variants_len: usize) -> u32 {
    // TODO: when bool-sub-byte packing is implemented, remove function in favor of enum_tag_size_used
    enum_tag_size_used(variants_len).div_ceil(8) * 8
}

fn enum_tag_size_used(variants_len: usize) -> u32 {
    f64::log2(variants_len as f64).ceil() as u32
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
