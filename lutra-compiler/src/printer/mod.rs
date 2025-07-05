use crate::pr;

pub fn print_ty(ty: &pr::Ty) -> String {
    let mut printer = Printer::default();

    printer.print_ty(ty)
}

#[derive(Clone, Default)]
struct Printer {
    indent: usize,
}

const INDENT: usize = 2;

#[allow(dead_code)]
impl Printer {
    fn indent(&mut self) {
        self.indent += INDENT;
    }

    fn dedent(&mut self) {
        self.indent -= INDENT;
    }

    fn new_line(&self) -> String {
        let mut r = "\n".to_string();
        r += &" ".repeat(self.indent);
        r
    }
}

impl Printer {
    #[allow(clippy::only_used_in_recursion)]
    fn print_ty(&mut self, ty: &pr::Ty) -> String {
        match &ty.kind {
            pr::TyKind::Ident(ident) => ident.to_string(),
            pr::TyKind::Primitive(prim) => prim.to_string(),
            pr::TyKind::Tuple(fields) => {
                let mut r = String::new();
                r += "{";
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        r += ", ";
                    }
                    if let Some(name) = &field.name {
                        r += &pr::display_ident(name);
                        r += ": ";
                    }
                    r += &self.print_ty(&field.ty);
                }
                r += "}";
                r
            }
            pr::TyKind::Array(item) => {
                let mut r = String::new();
                r += "[";
                r += &self.print_ty(item);
                r += "]";
                r
            }
            pr::TyKind::Enum(variants) => {
                let mut r = String::new();
                r += "enum {";
                for (i, variant) in variants.iter().enumerate() {
                    if i > 0 {
                        r += ", ";
                    }
                    r += &pr::display_ident(&variant.name);
                    let is_unit = variant.ty.kind.as_tuple().is_some_and(|f| f.is_empty());
                    if !is_unit {
                        r += ": ";
                        r += &self.print_ty(&variant.ty);
                    }
                }
                r += "}";
                r
            }
            pr::TyKind::Func(func) => {
                let mut r = String::new();
                r += "func";
                if !func.ty_params.is_empty() {
                    r += " <";
                    for (i, ty_param) in func.ty_params.iter().enumerate() {
                        if i > 0 {
                            r += ", ";
                        }

                        r += &ty_param.name;
                        match &ty_param.domain {
                            pr::TyParamDomain::Open => {}
                            pr::TyParamDomain::OneOf(tys) => {
                                for (i, ty) in tys.iter().enumerate() {
                                    if i > 0 {
                                        r += " | ";
                                    }
                                    r += &ty.to_string();
                                }
                            }
                            pr::TyParamDomain::TupleFields(fields) => {
                                r += ": {";
                                for (i, field) in fields.iter().enumerate() {
                                    if i > 0 {
                                        r += ", ";
                                    }
                                    match &field.location {
                                        pr::IndirectionKind::Name(name) => {
                                            r += &pr::display_ident(name);
                                            r += ": ";
                                        }
                                        pr::IndirectionKind::Position(p) => {
                                            assert_eq!(i, *p as usize); // TODO: print these fields when they are out of order
                                        }
                                    }
                                    r += &self.print_ty(&field.ty);
                                }
                                r += ", ..}";
                            }
                        }
                    }
                    r += ">";
                }
                r += " (";
                for (i, ty_param) in func.params.iter().enumerate() {
                    if i > 0 {
                        r += ", ";
                    }

                    if let Some(ty) = ty_param {
                        r += &self.print_ty(ty);
                    } else {
                        r += "?";
                    }
                }
                r += ")";
                if let Some(body) = &func.body {
                    r += ": ";
                    r += &self.print_ty(body);
                }

                r
            }
        }
    }
}
