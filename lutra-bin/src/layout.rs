use std::{
    collections::{HashMap, HashSet},
    ops::Mul,
};

use lutra_frontend::pr;

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

    enum_recursive_variants: HashMap<String, HashSet<usize>>,
}

impl LayoutCache {
    pub fn does_enum_variant_contain_recursive(
        &self,
        enum_ty: &pr::Ty,
        variant_index: usize,
    ) -> bool {
        let Some(enum_name) = &enum_ty.name else {
            return false; // TODO: is this ok?
        };

        let Some(recursive_variants) = self.enum_recursive_variants.get(enum_name) else {
            return false;
        };

        recursive_variants.contains(&variant_index)
    }
}

pub fn get_head_size<'t>(ty: &pr::Ty, cache: &mut LayoutCache) -> Result<usize> {
    let mut ctx = GetHeadSizeCtx::new(cache);
    ctx.get(ty)
}

struct GetHeadSizeCtx<'a> {
    cache: &'a mut LayoutCache,
    path: Vec<&'a pr::Ty>,
}

impl<'a> GetHeadSizeCtx<'a> {
    fn new(cache: &'a mut LayoutCache) -> Self {
        GetHeadSizeCtx {
            cache,
            path: Vec::new(),
        }
    }

    fn get(&mut self, ty: &'a pr::Ty) -> std::result::Result<usize, Error> {
        if let Some(name) = &ty.name {
            if let Some(existing) = self.cache.head_size_cache.get(name) {
                return Ok(*existing);
            }
        }
        self.path.push(ty);

        let size = match &ty.kind {
            pr::TyKind::Primitive(pr::PrimitiveSet::Bool) => 8,
            pr::TyKind::Primitive(pr::PrimitiveSet::Int) => 64,
            pr::TyKind::Primitive(pr::PrimitiveSet::Float) => 64,
            pr::TyKind::Primitive(pr::PrimitiveSet::Text) => 64,
            pr::TyKind::Array(_) => 64,

            pr::TyKind::Tuple(fields) => {
                let mut size = 0;
                for f in fields {
                    size += self.get(&f.ty)?;
                }
                size
            }
            pr::TyKind::Enum(variants) => {
                let head = self.enum_head_format(variants)?;
                if head.is_always_inline {
                    head.s + head.h
                } else {
                    head.s + 32
                }
            }

            pr::TyKind::Ident(ident) => {
                if let Some(val) = self.cache.head_size_cache.get(ident.name()) {
                    *val
                } else {
                    // if target of the ident has not yet been resolved, there are two cases:
                    // - resolution order is wrong (this results in an error for now),
                    // - the type is recursive, we need to check its validity

                    let (is_recursive, enum_ptr) = self.validate_recursive_type(ident.name())?;
                    if is_recursive {
                        if let Some((enum_name, variant_index)) = enum_ptr {
                            let entry = self
                                .cache
                                .enum_recursive_variants
                                .entry(enum_name)
                                .or_default();
                            entry.insert(variant_index);
                        }

                        32 // a pointer
                    } else {
                        return Err(Error::InvalidTypeReference {
                            name: ident.name().to_string(),
                        });
                    }
                }
            }

            pr::TyKind::Primitive(_) | pr::TyKind::Function(_) => return Err(Error::InvalidType),
        };

        self.path.pop().unwrap();

        if let Some(name) = &ty.name {
            self.cache.head_size_cache.insert(name.clone(), size);
        }
        Ok(size)
    }

    fn validate_recursive_type(&self, name: &str) -> Result<(bool, Option<(String, usize)>)> {
        let mut path = self.path.iter().rev();

        let mut prev = path.next().unwrap(); // current type
        let mut pointer_found = None;
        for t in path {
            if matches!(&t.kind, pr::TyKind::Array(_) | pr::TyKind::Enum(_)) {
                pointer_found = Some((t, prev));
            }
            if t.name.as_deref().map_or(false, |n| n == name) {
                // found it
                return if let Some((ptr_ty, ptr_child)) = pointer_found {
                    let enum_ptr = if let pr::TyKind::Enum(variants) = &ptr_ty.kind {
                        let ptr_enum_name = ptr_ty.name.clone().unwrap();
                        let variant = variants.iter().position(|(_, t)| t == *ptr_child).unwrap();

                        Some((ptr_enum_name, variant))
                    } else {
                        None
                    };

                    Ok((true, enum_ptr))
                } else {
                    Err(Error::InvalidTypeRecursive)
                };
            }
            prev = t;
        }
        Ok((false, None))
    }

    fn enum_head_format(&mut self, variants: &'a [(String, pr::Ty)]) -> Result<EnumHeadFormat> {
        let s = enum_tag_size(variants.len());

        let h = self.enum_max_variant_head_size(variants)?;

        Ok(EnumHeadFormat {
            s,
            h,
            is_always_inline: s + h <= 64,
        })
    }

    fn enum_variant_format(
        &mut self,
        head: &EnumHeadFormat,
        variant_ty: &'a pr::Ty,
    ) -> Result<EnumVariantFormat> {
        let variant_size = self.get(variant_ty)?;

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

    fn enum_max_variant_head_size(&mut self, variants: &'a [(String, pr::Ty)]) -> Result<usize> {
        let mut h = 0;
        for (_n, ty) in variants {
            let size = self.get(ty)?;
            h = h.max(size);
        }
        Ok(h)
    }
}

pub struct EnumHeadFormat {
    pub s: usize,
    pub h: usize,
    pub is_always_inline: bool,
}

pub fn enum_head_format<'t>(
    variants: &'t [(String, pr::Ty)],
    cache: &'t mut LayoutCache,
) -> Result<EnumHeadFormat> {
    let mut ctx = GetHeadSizeCtx::new(cache);
    ctx.enum_head_format(variants)
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
    let mut ctx = GetHeadSizeCtx::new(cache);
    ctx.enum_variant_format(head, variant_ty)
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
