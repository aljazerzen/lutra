use crate::string;
use crate::vec;

use crate::ReaderExt;
use crate::ir;

/// Size of a pointer to a body, in bytes.
pub const PTR_BYTES: u32 = 4;

/// Size of the head of an array (body pointer + length), in bytes.
pub const ARRAY_HEAD_BYTES: u32 = 2 * PTR_BYTES;

/// Largest inner head, in bits, that is still stored inline in a enum type head.
/// Anything larger is moved to the body and replaced by a pointer.
pub const ENUM_INLINE_MAX_BITS: u32 = 8 * PTR_BYTES;

// TODO: conceptually, this shouldn't exist
pub trait Layout {
    /// Returns the size of the head in bits for a given type.
    fn head_size() -> usize;
}

impl<T> Layout for &T
where
    T: Layout,
{
    fn head_size() -> usize {
        T::head_size()
    }
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
        option_head_format::<I>().head_size() as usize
    }
}

impl<T: Layout, E: Layout> Layout for core::result::Result<T, E> {
    fn head_size() -> usize {
        result_head_format::<T, E>().head_size() as usize
    }
}

/// Head format of the built-in `Option<I>`, which is a two-variant enum
/// with a unit variant and `I`.
pub fn option_head_format<I: Layout>() -> EnumHeadFormat {
    EnumHeadFormat::new(enum_tag_size(2), I::head_size() as u32, false)
}

/// Head format of the built-in `Result<T, E>`, which is a two-variant enum.
pub fn result_head_format<T: Layout, E: Layout>() -> EnumHeadFormat {
    let max_head = (T::head_size() as u32).max(E::head_size() as u32);
    EnumHeadFormat::new(enum_tag_size(2), max_head, false)
}

impl Layout for () {
    fn head_size() -> usize {
        0
    }
}

/// Computes layout of a type, iff all inner types have their layout computed.
// Does not recurse.
pub fn compute(ty: &ir::Ty) -> Option<ir::TyLayout> {
    Some(match &ty.kind {
        ir::TyKind::Array(_) => ir::TyLayout {
            head_size: ARRAY_HEAD_BYTES * 8,
            body_ptrs: vec![0],
        },

        ir::TyKind::Primitive(prim) => {
            let head_size = match prim {
                ir::TyPrimitive::Prim8 => 8,
                ir::TyPrimitive::Prim16 => 16,
                ir::TyPrimitive::Prim32 => 32,
                ir::TyPrimitive::Prim64 => 64,
            };
            ir::TyLayout {
                head_size,
                body_ptrs: vec![],
            }
        }

        ir::TyKind::Tuple(fields) => {
            let mut head_size: u32 = 0;
            let mut body_ptrs = vec::Vec::new();
            for f in fields {
                let Some(layout) = &f.ty.layout else {
                    return None;
                };

                let field_offset = head_size.div_ceil(8);
                head_size += layout.head_size;
                body_ptrs.extend(layout.body_ptrs.iter().map(|p| p + field_offset));
            }

            ir::TyLayout {
                head_size,
                body_ptrs,
            }
        }

        ir::TyKind::Enum(variants) => {
            let head = enum_head_format(variants, &ty.variants_recursive);

            let head_size = head.head_size();

            let body_ptrs = if head.has_ptr {
                vec![head.tag_bytes]
            } else {
                // here we should include body_ptrs in the head of inner type,
                // but no type with a ptr fits into 4 bytes, so we can just skip
                // that and assume there are no body_ptrs
                vec![]
            };

            ir::TyLayout {
                head_size,
                body_ptrs,
            }
        }
        _ => return None,
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

pub use crate::generated::layout::EnumFormat;

impl EnumFormat {
    /// The head format, shared by all variants.
    pub fn head_format(&self) -> EnumHeadFormat {
        EnumHeadFormat {
            tag_bytes: self.tag_bytes as u32,
            inner_bytes: self.inner_bytes as u32,
            has_ptr: self.has_ptr,
        }
    }

    /// Resolves the head of the variant selected by `tag`.
    pub fn variant(&self, tag: u64) -> EnumVariantHead {
        self.head_format().variant(tag, self.variant_head_bits(tag))
    }

    /// Reads the tag and returns the buffer positioned at the head of the
    /// inner value.
    pub fn decode_head<'b>(&self, buf: &'b [u8]) -> (u64, &'b [u8]) {
        let tag = self.head_format().decode_tag(buf);
        (tag, self.variant(tag).decode_inner(buf))
    }

    /// Head size of the given variant, in bits, rounded up to whole bytes.
    fn variant_head_bits(&self, tag: u64) -> u32 {
        let variant = &self.variants[tag as usize];
        if variant.is_unit {
            0
        } else if self.has_ptr {
            // stored out-of-line: the exact size is irrelevant, it only has to
            // be larger than what fits inline
            ENUM_INLINE_MAX_BITS + 8
        } else {
            (self.inner_bytes - variant.padding_bytes) as u32 * 8
        }
    }
}

pub fn enum_format(variants: &[ir::TyEnumVariant], variants_recursive: &[u16]) -> EnumFormat {
    let head = enum_head_format(variants, variants_recursive);
    let variants = variants
        .iter()
        .map(|v| enum_variant_format(&head, &v.ty))
        .collect();
    EnumFormat {
        tag_bytes: head.tag_bytes as u8,
        inner_bytes: head.inner_bytes as u8,
        has_ptr: head.has_ptr,
        variants,
    }
}

#[derive(Debug, Clone, Copy)]
pub struct EnumHeadFormat {
    pub tag_bytes: u32,
    pub inner_bytes: u32,
    pub has_ptr: bool,
}

impl EnumHeadFormat {
    /// The single definition of the enum head rule:
    /// tag, followed by the inner head inline, or by a pointer to the body
    /// when the inner head does not fit into [ENUM_INLINE_MAX_BITS].
    pub const fn new(tag_bits: u32, max_variant_head_bits: u32, force_ptr: bool) -> EnumHeadFormat {
        let has_ptr = force_ptr || max_variant_head_bits > ENUM_INLINE_MAX_BITS;
        let inner_bits = if has_ptr {
            ENUM_INLINE_MAX_BITS
        } else {
            max_variant_head_bits
        };

        EnumHeadFormat {
            tag_bytes: tag_bits.div_ceil(8),
            inner_bytes: inner_bits.div_ceil(8),
            has_ptr,
        }
    }

    /// Size of the head in bits.
    pub const fn head_size(&self) -> u32 {
        (self.tag_bytes + self.inner_bytes) * 8
    }

    /// Resolves the rules for one variant, given the head size of its inner
    /// value in bits.
    pub const fn variant(&self, tag: u64, variant_head_bits: u32) -> EnumVariantHead {
        // a variant whose inner head is empty is stored without a pointer:
        // there is no body to point to
        let has_ptr = self.has_ptr && variant_head_bits > 0;

        let padding_bytes = if self.has_ptr {
            if has_ptr {
                0
            } else {
                // unit variant does not have a pointer, it is all padding
                self.inner_bytes
            }
        } else {
            self.inner_bytes
                .saturating_sub(variant_head_bits.div_ceil(8))
        };

        EnumVariantHead {
            tag,
            tag_bytes: self.tag_bytes,
            has_ptr,
            padding_bytes,
        }
    }

    /// Reads the tag of an encoded head.
    pub fn decode_tag(&self, buf: &[u8]) -> u64 {
        let mut tag = [0_u8; 8];
        tag[0..self.tag_bytes as usize].copy_from_slice(buf.read_n(self.tag_bytes as usize));
        u64::from_le_bytes(tag)
    }
}

/// Head of one variant of an enum: everything needed to encode or decode it.
#[derive(Debug, Clone, Copy)]
pub struct EnumVariantHead {
    pub tag: u64,
    pub tag_bytes: u32,
    /// Whether the inner value is stored in the body, behind a pointer.
    pub has_ptr: bool,
    /// Number of zero bytes that follow the inner head (or the pointer),
    /// so that all variants have the same head size.
    pub padding_bytes: u32,
}

impl EnumVariantHead {
    /// Encodes the tag and, if the inner value is stored in the body,
    /// reserves the pointer to it.
    ///
    /// When this returns `None`, the head of the inner value must follow
    /// directly. In both cases, [EnumVariantHead::encode_padding] must be
    /// called afterwards.
    pub fn encode_tag(&self, buf: &mut bytes::BytesMut) -> Option<crate::ReversePointer> {
        use bytes::BufMut;

        buf.put_slice(&self.tag.to_le_bytes()[0..self.tag_bytes as usize]);

        if self.has_ptr {
            Some(crate::ReversePointer::new(buf))
        } else {
            None
        }
    }

    /// Pads the head, so that all variants have the same head size.
    pub fn encode_padding(&self, buf: &mut bytes::BytesMut) {
        use bytes::BufMut;

        if self.padding_bytes > 0 {
            buf.put_bytes(0, self.padding_bytes as usize);
        }
    }

    /// Skips the tag of an encoded head and returns the buffer positioned at
    /// the head of the inner value, dereferencing the body pointer if there is one.
    pub fn decode_inner<'b>(&self, buf: &'b [u8]) -> &'b [u8] {
        let buf = buf.skip(self.tag_bytes as usize);

        if self.has_ptr {
            let offset = u32::from_le_bytes(buf.read_const::<4>()) as usize;
            buf.skip(offset)
        } else {
            // the inner head follows the tag directly
            buf
        }
    }
}

pub fn enum_head_format(
    variants: &[ir::TyEnumVariant],
    variants_recursive: &[u16],
) -> EnumHeadFormat {
    let force_ptr = !variants_recursive.is_empty();
    let max_head = if force_ptr {
        0
    } else {
        enum_max_variant_head_size(variants)
    };

    EnumHeadFormat::new(enum_tag_size(variants.len()), max_head, force_ptr)
}

pub use crate::generated::layout::EnumVariantFormat;

pub fn enum_variant_format(head: &EnumHeadFormat, variant_ty: &ir::Ty) -> EnumVariantFormat {
    let inner_head_size = variant_ty.layout.as_ref().unwrap().head_size;

    EnumVariantFormat {
        padding_bytes: head.variant(0, inner_head_size).padding_bytes as u8,
        is_unit: inner_head_size == 0,
    }
}

fn enum_max_variant_head_size(variants: &[ir::TyEnumVariant]) -> u32 {
    let mut i = 0;
    for v in variants {
        let Some(ty_layout) = &v.ty.layout else {
            panic!("missing layout: {:?}", v.ty)
        };
        let size = ty_layout.head_size;
        i = i.max(size);
    }
    i
}

pub fn enum_tag_size(variants_len: usize) -> u32 {
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

#[test]
fn test_variant_head_bits() {
    use crate::string::ToString;

    fn variant(name: &str, ty: ir::Ty) -> ir::TyEnumVariant {
        let mut ty = ty;
        ty.layout = compute(&ty);
        ir::TyEnumVariant {
            name: name.to_string(),
            ty,
        }
    }
    fn prim(p: ir::TyPrimitive) -> ir::Ty {
        ir::Ty::new(ir::TyKind::Primitive(p))
    }

    let cases = [
        // inline: all variants fit into the inner head
        vec![
            variant("a", ir::Ty::new_unit()),
            variant("b", prim(ir::TyPrimitive::Prim16)),
            variant("c", prim(ir::TyPrimitive::Prim32)),
        ],
        // out-of-line: the largest variant does not fit
        vec![
            variant("a", ir::Ty::new_unit()),
            variant("b", prim(ir::TyPrimitive::Prim64)),
        ],
        // unit only
        vec![
            variant("a", ir::Ty::new_unit()),
            variant("b", ir::Ty::new_unit()),
        ],
    ];

    for variants in cases {
        let format = enum_format(&variants, &[]);
        let head = format.head_format();

        for (tag, variant) in variants.iter().enumerate() {
            // resolved from the type, as the compiler and codegen do
            let from_ty = head.variant(tag as u64, variant.ty.layout.as_ref().unwrap().head_size);
            // resolved from the serialized format, as the runners do
            let from_format = format.variant(tag as u64);

            assert_eq!(from_ty.has_ptr, from_format.has_ptr, "{variants:?}");
            assert_eq!(
                from_ty.padding_bytes, from_format.padding_bytes,
                "{variants:?}"
            );
            assert_eq!(
                from_format.padding_bytes, format.variants[tag].padding_bytes as u32,
                "{variants:?}"
            );
        }
    }
}
