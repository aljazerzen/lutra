use crate::decl;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::Result;

use super::Resolver;

impl Resolver<'_> {
    pub fn compute_ty_layout(&mut self, ty: &pr::Ty) -> Result<Option<pr::TyLayout>, Diagnostic> {
        if ty.layout.is_some() {
            return Ok(ty.layout.clone());
        }

        let head_size = match &ty.kind {
            pr::TyKind::Primitive(_) | pr::TyKind::Array(_) | pr::TyKind::Tuple(_) => {
                if let Some(size) = ty.kind.get_head_size() {
                    size
                } else {
                    return Ok(None);
                }
            }

            pr::TyKind::Enum(variants) => {
                let head_size = ty.kind.get_head_size().unwrap();

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
                        return Err(Diagnostic::new_custom(format!(
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
