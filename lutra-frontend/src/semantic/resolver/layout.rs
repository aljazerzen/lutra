use crate::diagnostic::Diagnostic;
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
                let decl = self.try_resolve_ty_ident(&*ty)?;

                match decl {
                    // resolves to a type: use it's layout
                    Some(ty) => {
                        if let Some(layout) = &ty.layout {
                            layout.clone()
                        } else if self.strict_mode {
                            panic!("Unresolved layout of reference {ident} at {:?}: {ty:?} (during eval of {})", ty.span, self.debug_current_decl)
                        } else {
                            return Ok(());
                        }
                    }

                    // unresolved: recursive reference to a type
                    None => {
                        if self.strict_mode {
                            panic!(
                                "unresolved {ident} at {:?}: (during eval of {})",
                                ty.span, self.debug_current_decl
                            )
                        }
                        return Ok(());
                    }
                }
            }

            // functions cannot be serialized, so we can dream up a layout
            pr::TyKind::Function(_) => pr::TyLayout {
                head_size: 0,
                body_ptrs: vec![],
                variants_recursive: vec![],
            },
        };
        ty.layout = Some(layout);
        Ok(())
    }
}
