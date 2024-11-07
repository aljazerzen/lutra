use crate::error::{Diagnostic, WithErrorInfo};
use crate::ir::decl;
use crate::pr;
use crate::Result;

use super::Resolver;

impl Resolver<'_> {
    pub fn compute_ty_layout(&mut self, ty: &pr::Ty) -> Result<Option<pr::TyLayout>, Diagnostic> {
        if ty.layout.is_some() {
            return Ok(ty.layout.clone());
        }

        let head_size = match &ty.kind {
            pr::TyKind::Primitive(prim) => match prim {
                pr::PrimitiveSet::Bool => 8,
                pr::PrimitiveSet::Int => 64,
                pr::PrimitiveSet::Float => 64,
                pr::PrimitiveSet::Text => 64,
                _ => unimplemented!(),
            },
            pr::TyKind::Array(_) => 64,

            pr::TyKind::Tuple(fields) => {
                let mut size = 0;
                for f in fields {
                    if let Some(layout) = &f.ty.layout {
                        size += layout.head_size;
                    } else {
                        return Ok(None);
                    }
                }
                size
            }
            pr::TyKind::Enum(variants) => {
                let tag_size = enum_tag_size(variants.len());

                let head_size = tag_size + 32;

                let mut variants_recursive = Vec::new();
                for (index, (_, variant_ty)) in variants.iter().enumerate() {
                    if variant_ty.layout.is_none() {
                        // unresolved - this type is (probably) recursive, save this info
                        // (I don't think this logic is 100% sound)
                        variants_recursive.push(index);
                    }
                }

                return Ok(Some(pr::TyLayout {
                    head_size,
                    variants_recursive,
                }));
            }

            pr::TyKind::Ident(ident) => {
                let decl = self.get_ident(ident).ok_or_else(|| {
                    Diagnostic::new_assert("cannot find type ident")
                        .push_hint(format!("ident={ident:?}"))
                })?;

                match &decl.kind {
                    // reference to a type: use it's layout
                    decl::DeclKind::Ty(ty) => {
                        if let Some(layout) = &ty.layout {
                            layout.head_size
                        } else if self.strict_mode {
                            panic!("Unresolved layout of reference {ident} at {:?}: {ty:?} (during eval of {})", ty.span, self.debug_current_decl)
                        } else {
                            return Ok(None);
                        }
                    }

                    // recursive reference to a type
                    decl::DeclKind::Unresolved(u) => {
                        if self.strict_mode {
                            panic!(
                                "Unresolved {ident} at {:?}: {u:?} (during eval of {})",
                                ty.span, self.debug_current_decl
                            )
                        }
                        return Ok(None);
                    }

                    _ => {
                        return Err(Diagnostic::new_simple(format!(
                            "expected a type, but found {decl:?}"
                        ))
                        .with_span(ty.span))
                    }
                }
            }

            pr::TyKind::Function(_) => unimplemented!(),
        };
        Ok(Some(pr::TyLayout {
            head_size,
            variants_recursive: vec![],
        }))
    }
}

fn enum_tag_size(variants_len: usize) -> usize {
    // TODO: when bool-sub-byte packing is implemented, remove function in favor of enum_tag_size_used
    enum_tag_size_used(variants_len).div_ceil(8) * 8
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
