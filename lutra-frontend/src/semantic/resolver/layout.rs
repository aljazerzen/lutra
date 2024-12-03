use crate::decl;
use crate::diagnostic::{Diagnostic, WithErrorInfo};
use crate::pr;
use crate::Result;

use super::Resolver;

impl Resolver<'_> {
    pub fn compute_ty_layout(&mut self, ty: &mut pr::Ty) -> Result<(), Diagnostic> {
        if ty.layout.is_some() {
            return Ok(());
        }

        let layout = match &ty.kind {
            pr::TyKind::Primitive(_) | pr::TyKind::Array(_) | pr::TyKind::Tuple(_) => {
                if let Some(layout) = ty.kind.get_layout_simple() {
                    layout
                } else {
                    return Ok(());
                }
            }

            pr::TyKind::Enum(variants) => {
                let mut layout = ty.kind.get_layout_simple().unwrap();

                assert!(layout.variants_recursive.is_empty());
                for (index, variant) in variants.iter().enumerate() {
                    if variant.ty.layout.is_none() {
                        // unresolved - this type is (probably) recursive, save this info
                        // (I don't think this logic is 100% sound)
                        layout.variants_recursive.push(index as u16);
                    }
                }

                ty.layout = Some(layout);
                return Ok(());
            }

            pr::TyKind::Ident(ident) => {
                let decl = self.get_ident(ident).ok_or_else(|| {
                    Diagnostic::new_assert("cannot find type ident")
                        .push_hint(format!("ident={ident:?}"))
                        .with_span(ty.span)
                })?;

                match &decl.kind {
                    // reference to a type: use it's layout
                    decl::DeclKind::Ty(ty) => {
                        if let Some(layout) = &ty.layout {
                            layout.clone()
                        } else if self.strict_mode {
                            panic!("Unresolved layout of reference {ident} at {:?}: {ty:?} (during eval of {})", ty.span, self.debug_current_decl)
                        } else {
                            return Ok(());
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
                        return Ok(());
                    }

                    _ => {
                        return Err(Diagnostic::new_custom(format!(
                            "expected a type, but found {decl:?}"
                        ))
                        .with_span(ty.span))
                    }
                }
            }

            // functions cannot be serialized, so we can dream up a layout
            pr::TyKind::Function(_) => pr::TyLayout {
                head_size: 0,
                body_ptr_offset: None,
                variants_recursive: vec![],
            },
        };
        ty.layout = Some(layout);
        Ok(())
    }
}
