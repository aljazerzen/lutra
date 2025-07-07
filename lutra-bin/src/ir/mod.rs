mod literal;
mod printer;

pub use crate::generated::ir::*;
#[cfg(feature = "std")]
pub use printer::{print, print_ty};

use crate::{boxed, string, vec};

impl Program {
    pub fn get_output_ty(&self) -> &Ty {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        &main_ty.body
    }

    pub fn get_input_ty(&self) -> &Ty {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        assert_eq!(main_ty.params.len(), 1);
        &main_ty.params[0]
    }
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Eq for Ty {}

impl Ty {
    pub fn new(kind: impl Into<TyKind>) -> Self {
        Ty {
            kind: kind.into(),
            layout: None,
            name: None,
            variants_recursive: vec![],
        }
    }
    pub fn new_unit() -> Self {
        Ty {
            kind: TyKind::Tuple(vec![]),
            layout: Some(TyLayout {
                head_size: 0,
                body_ptrs: vec![],
            }),
            name: None,
            variants_recursive: vec![],
        }
    }
    pub fn is_unit(&self) -> bool {
        self.kind.as_tuple().is_some_and(|f| f.is_empty())
    }

    pub fn iter_fields<'a>(&'a self) -> boxed::Box<dyn Iterator<Item = &'a Ty> + 'a> {
        match &self.kind {
            TyKind::Tuple(fields) => boxed::Box::new(fields.iter().map(|f| &f.ty)),
            _ => boxed::Box::new(Some(self).into_iter()),
        }
    }
}

impl From<TyPrimitive> for TyKind {
    fn from(value: TyPrimitive) -> Self {
        TyKind::Primitive(value)
    }
}
impl From<vec::Vec<TyTupleField>> for TyKind {
    fn from(value: vec::Vec<TyTupleField>) -> Self {
        TyKind::Tuple(value)
    }
}
impl From<TyFunction> for TyKind {
    fn from(value: TyFunction) -> Self {
        TyKind::Function(boxed::Box::new(value))
    }
}
impl From<Path> for TyKind {
    fn from(value: Path) -> Self {
        TyKind::Ident(value)
    }
}

impl Module {
    pub fn insert(&mut self, path: &[string::String], decl: Decl) {
        if path.is_empty() {
            panic!();
        }

        if path.len() == 1 {
            self.decls.retain(|d| d.name != path[0]);
            self.decls.push(ModuledeclsItems {
                name: path[0].clone(),
                decl,
            });
        } else {
            let exists = self.decls.iter().any(|d| d.name == path[0]);
            if !exists {
                self.decls.push(ModuledeclsItems {
                    name: path[0].clone(),
                    decl: Decl::Module(boxed::Box::new(Module {
                        decls: vec::Vec::new(),
                    })),
                });
            }

            let sub_module = self.decls.iter_mut().find(|d| d.name == path[0]);
            let Decl::Module(sub_module) = &mut sub_module.unwrap().decl else {
                panic!()
            };
            sub_module.insert(&path[1..], decl)
        }
    }

    pub fn iter_decls_re(&self) -> impl Iterator<Item = (Path, &Decl)> {
        self.decls.iter().flat_map(|item| match &item.decl {
            Decl::Module(sub_module) => sub_module
                .iter_decls_re()
                .map(|(mut p, d)| {
                    p.0.insert(0, item.name.clone());
                    (p, d)
                })
                .collect::<vec::Vec<_>>(),
            _ => {
                vec![(Path(vec![item.name.clone()]), &item.decl)]
            }
        })
    }

    pub fn iter_types_re(&self) -> impl Iterator<Item = (Path, &Ty)> {
        self.iter_decls_re().filter_map(|(p, d)| {
            if let Decl::Type(ty) = d {
                Some((p, ty))
            } else {
                None
            }
        })
    }
}
