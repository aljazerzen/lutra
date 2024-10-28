use lutra_parser::parser::pr;

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

pub fn get_head_size(ty: &pr::Ty) -> usize {
    match &ty.kind {
        pr::TyKind::Primitive(pr::PrimitiveSet::Bool) => 8,
        pr::TyKind::Primitive(pr::PrimitiveSet::Int) => 64,
        pr::TyKind::Primitive(pr::PrimitiveSet::Float) => 64,
        pr::TyKind::Primitive(pr::PrimitiveSet::Text) => 64,
        pr::TyKind::Array(_) => 64,

        pr::TyKind::Tuple(fields) => fields.iter().map(|f| get_head_size(&f.ty)).sum(),
        pr::TyKind::Enum(variants) => {
            let s = enum_tag_size(variants.len());

            let h = variants
                .iter()
                .map(|(_n, ty)| get_head_size(ty))
                .max()
                .unwrap_or_default();

            if s + h <= 64 {
                s + h
            } else {
                s + 32
            }
        }

        pr::TyKind::Primitive(_) => todo!(),
        pr::TyKind::Function(_) => todo!(),
        pr::TyKind::Ident(_) => todo!(),
    }
}

fn enum_tag_size(variants_len: usize) -> usize {
    f64::log2(variants_len as f64).ceil() as usize
}

#[test]
fn test_enum_tag_size() {
    assert_eq!(0, enum_tag_size(0));
    assert_eq!(0, enum_tag_size(1));
    assert_eq!(1, enum_tag_size(2));
    assert_eq!(2, enum_tag_size(3));
    assert_eq!(2, enum_tag_size(4));
    assert_eq!(3, enum_tag_size(5));
    assert_eq!(3, enum_tag_size(6));
    assert_eq!(3, enum_tag_size(7));
    assert_eq!(3, enum_tag_size(8));
    assert_eq!(4, enum_tag_size(9));
    assert_eq!(4, enum_tag_size(10));
    assert_eq!(4, enum_tag_size(11));
    assert_eq!(4, enum_tag_size(12));
    assert_eq!(4, enum_tag_size(13));
    assert_eq!(4, enum_tag_size(14));
    assert_eq!(4, enum_tag_size(15));
    assert_eq!(4, enum_tag_size(16));
    assert_eq!(5, enum_tag_size(17));
    assert_eq!(5, enum_tag_size(18));
    assert_eq!(5, enum_tag_size(19));
    assert_eq!(5, enum_tag_size(20));
    assert_eq!(5, enum_tag_size(21));
    assert_eq!(5, enum_tag_size(22));
}
