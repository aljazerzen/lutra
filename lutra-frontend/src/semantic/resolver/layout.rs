use crate::decl;
use crate::diagnostic::Diagnostic;
use crate::diagnostic::WithErrorInfo;
use crate::pr;
use crate::Result;

use super::scope::Named;
use super::scope::ScopedKind;
use super::Resolver;

impl Resolver<'_> {
    pub fn compute_ty_layout(&mut self, ty: &mut pr::Ty) -> Result<bool, Diagnostic> {
        if ty.layout.is_some() {
            return Ok(false);
        }

        let layout = match &ty.kind {
            pr::TyKind::Primitive(_) | pr::TyKind::Array(_) | pr::TyKind::Tuple(_) => {
                if let Some(layout) = ty.kind.get_layout_simple() {
                    layout
                } else {
                    return Ok(false);
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
                return Ok(false);
            }

            pr::TyKind::Ident(ident) => {
                let named = self.get_ident(ident).ok_or_else(|| {
                    Diagnostic::new_assert("cannot find type ident")
                        .push_hint(format!("ident={ident:?}"))
                        .push_hint("compute_ty_layout")
                        .with_span(ty.span)
                })?;
                let ty = match &named {
                    Named::Decl(decl) => match &decl.kind {
                        decl::DeclKind::Ty(ty) => ty,

                        // unresolved: recursive reference to a type
                        decl::DeclKind::Unresolved(_) => {
                            if self.strict_mode {
                                panic!(
                                    "unresolved {ident} at {:?}: (during eval of {})",
                                    ty.span, self.debug_current_decl
                                )
                            }
                            return Ok(true);
                        }
                        _ => {
                            return Err(Diagnostic::new_assert("expected reference to a type")
                                .push_hint(format!("got {:?}", &decl.kind))
                                .with_span(ty.span))
                        }
                    },
                    Named::Scoped(scoped) => match scoped {
                        ScopedKind::Param { ty } => {
                            return Err(Diagnostic::new_assert("expected reference to a type")
                                .push_hint(format!("got {:?}", &scoped))
                                .with_span(ty.span))
                        }
                        ScopedKind::Type { ty } => ty,
                        ScopedKind::TypeParam { .. } | ScopedKind::TypeArg { .. } => {
                            // generic type params do not have a layout
                            return Ok(false); // not missing
                        }
                    },
                };
                // resolves to a type: use it's layout
                if let Some(layout) = &ty.layout {
                    layout.clone()
                } else if self.strict_mode {
                    panic!("Unresolved layout of reference {ident} at {:?}: {ty:?} (during eval of {})", ty.span, self.debug_current_decl)
                } else {
                    return Ok(true);
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
        Ok(false)
    }
}
