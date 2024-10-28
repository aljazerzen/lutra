use std::{borrow::Cow, fmt::Write};

use lutra_parser::parser::pr;

pub fn codegen(source: &str) -> Result<String, std::fmt::Error> {
    let lr = lutra_parser::lexer::lex_source(source).unwrap();
    let (stmts, errs) = lutra_parser::parser::parse_lr_to_pr(0, lr.0);
    if !errs.is_empty() {
        panic!("parse errors: {errs:?}");
    }
    let mut stmts = stmts.unwrap();

    let mut w = String::new();
    for stmt in &mut stmts {
        let mut ctx = Context::default();

        match &mut stmt.kind {
            pr::StmtKind::QueryDef(_) => todo!(),
            pr::StmtKind::VarDef(_) => todo!(),
            pr::StmtKind::ModuleDef(_) => todo!(),
            pr::StmtKind::ImportDef(_) => todo!(),
            pr::StmtKind::TypeDef(ty_def) => {
                infer_names(ty_def);

                let ty = ty_def.value.as_ref().unwrap();
                write_ty_def(&mut w, ty, &mut ctx)?;
            }
        }

        while !ctx.def_buffer.is_empty() {
            let ty = ctx.def_buffer.remove(0);
            write_ty_def(&mut w, ty, &mut ctx)?;
        }
    }
    Ok(w)
}

/// Types might not have names, because they are defined inline.
/// This function traverses a type definition and generates names for all of the types.
fn infer_names(ty_def: &mut pr::TypeDef) {
    let ty = ty_def.value.as_mut().unwrap();
    if ty.name.is_none() {
        ty.name = Some(ty_def.name.clone());
    }

    let mut name_prefix = Vec::new();
    infer_names_re(ty, &mut name_prefix);
}

fn infer_names_re(ty: &mut pr::Ty, name_prefix: &mut Vec<String>) {
    if ty.name.is_none() {
        ty.name = Some(name_prefix.concat());
    } else {
        name_prefix.push(ty.name.clone().unwrap());
    }

    match &mut ty.kind {
        pr::TyKind::Primitive(_) => {}

        pr::TyKind::Tuple(fields) => {
            for (index, field) in fields.iter_mut().enumerate() {
                let name = tuple_field_name(&field.name, index);
                name_prefix.push(name.into_owned());

                infer_names_re(&mut field.ty, name_prefix);
                name_prefix.pop();
            }
        }

        pr::TyKind::Array(items_ty) => {
            name_prefix.push("Items".to_string());
            infer_names_re(items_ty, name_prefix);
            name_prefix.pop();
        }

        _ => unimplemented!(),
    }
}

#[derive(Default)]
struct Context<'t> {
    /// Buffer for types that need their definitions generated.
    def_buffer: Vec<&'t pr::Ty>,
}

/// Generates a type definition.
fn write_ty_def<'t>(
    w: &mut impl Write,
    ty: &'t pr::Ty,
    ctx: &mut Context<'t>,
) -> Result<(), std::fmt::Error> {
    let name = ty.name.as_ref().unwrap();

    writeln!(w, "#[derive(Debug, Clone)]")?;
    writeln!(w, "#[allow(non_camel_case_types)]")?;
    match &ty.kind {
        pr::TyKind::Primitive(_) | pr::TyKind::Array(_) => {
            // generate a wrapper new-type struct
            write!(w, "pub struct {}(pub ", name)?;
            write_ty_ref(w, ty, false, ctx)?;
            writeln!(w, ");\n")?;
        }

        pr::TyKind::Tuple(fields) => {
            writeln!(w, "pub struct {} {{", name)?;

            for (index, field) in fields.iter().enumerate() {
                let name = tuple_field_name(&field.name, index);

                write!(w, "    pub {name}: ")?;
                write_ty_ref(w, &field.ty, false, ctx)?;

                writeln!(w, ",")?;
            }

            writeln!(w, "}}\n")?;
        }

        _ => unimplemented!(),
    }

    write_ty_def_impl(w, ty)?;

    Ok(())
}

fn tuple_field_name(name: &Option<String>, index: usize) -> Cow<'_, str> {
    (name.as_ref())
        .map(|x| Cow::Borrowed(x.as_str()))
        .unwrap_or_else(|| format!("field{index}").into())
}

/// Generates a reference to a type.
/// Syntactically, this could be used in `let x: type_ref`.
fn write_ty_ref<'t>(
    w: &mut impl Write,
    ty: &'t pr::Ty,
    as_expr: bool,
    ctx: &mut Context<'t>,
) -> Result<(), std::fmt::Error> {
    match &ty.kind {
        pr::TyKind::Primitive(pr::PrimitiveSet::Int) => {
            write!(w, "i64")?;
        }
        pr::TyKind::Primitive(pr::PrimitiveSet::Float) => {
            write!(w, "f64")?;
        }
        pr::TyKind::Primitive(pr::PrimitiveSet::Bool) => {
            write!(w, "bool")?;
        }
        pr::TyKind::Primitive(pr::PrimitiveSet::Text) => {
            write!(w, "String")?;
        }
        pr::TyKind::Array(items_ty) => {
            write!(w, "Vec")?;
            if as_expr {
                write!(w, "::<")?;
            } else {
                write!(w, "<")?;
            }
            write_ty_ref(w, items_ty, as_expr, ctx)?;
            write!(w, ">")?;
        }

        pr::TyKind::Tuple(_) => {
            ctx.def_buffer.push(ty);

            let name = ty.name.as_ref().unwrap();
            write!(w, "{name}")?;
        }

        _ => unimplemented!(),
    }
    Ok(())
}

/// Generates the impl encode for a type.
fn write_ty_def_impl<'t>(w: &mut impl Write, ty: &'t pr::Ty) -> Result<(), std::fmt::Error> {
    let name = ty.name.as_ref().unwrap();

    match &ty.kind {
        pr::TyKind::Primitive(_) | pr::TyKind::Array(_) => {
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type BodyMeta = ();")?;
            writeln!(w, "    fn encode_body(&self, w: &mut Vec<u8>)")?;
            writeln!(w, "        -> std::io::Result<()> {{")?;
            writeln!(w, "        Ok(())")?;
            writeln!(w, "    }}")?;
            writeln!(w, "    fn encode_head(&self, _: (), w: &mut Vec<u8>)")?;
            writeln!(w, "        -> std::io::Result<()> {{")?;
            writeln!(w, "        self.0.encode_head((), w)")?;
            writeln!(w, "    }}")?;
            writeln!(w, "}}")?;
        }

        pr::TyKind::Tuple(fields) => {
            writeln!(w, "impl ::lutra_bin::Encode for {name} {{")?;
            writeln!(w, "    type BodyMeta = {name}BodyMeta;")?;

            // encode body
            writeln!(w, "    fn encode_body(&self, w: &mut Vec<u8>)")?;
            writeln!(w, "        -> std::io::Result<Self::BodyMeta> {{")?;

            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "        let {0} = self.{0}.encode_body(w)?;", field_name)?;
            }

            writeln!(w, "        Ok({name}BodyMeta {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "            {field_name},")?;
            }
            writeln!(w, "        }})")?;
            writeln!(w, "    }}")?;

            // encode head
            writeln!(
                w,
                "    fn encode_head(&self, meta: Self::BodyMeta, w: &mut Vec<u8>)"
            )?;
            writeln!(w, "        -> std::io::Result<()> {{")?;

            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                writeln!(w, "        self.{0}.encode_head(meta.{0}, w)?;", field_name)?;
            }
            writeln!(w, "        Ok(())")?;
            writeln!(w, "    }}")?;

            writeln!(w, "}}")?;
            

            // body meta struct
            writeln!(w, "#[allow(non_camel_case_types)]")?;
            writeln!(w, "pub struct {name}BodyMeta {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);

                write!(w, "    {field_name}: <")?;

                let mut ctx = Context::default();
                write_ty_ref(w, &field.ty, true, &mut ctx)?;

                writeln!(w, " as ::lutra_bin::Encode>::BodyMeta,")?;
            }
            writeln!(w, "}}")?;
        }

        _ => unimplemented!(),
    }

    writeln!(w, "impl ::lutra_bin::Layout for {name} {{")?;
    writeln!(w, "    fn head_size() -> usize {{")?;
    match &ty.kind {
        pr::TyKind::Primitive(_) | pr::TyKind::Array(_) => {
            writeln!(w, "        self.0.head_size(r)")?;
        }

        pr::TyKind::Tuple(fields) => {
            let head_size: usize = fields.iter().map(|f| lutra_bin::get_head_size(&f.ty)).sum();
            writeln!(w, "        {head_size}")?;
        }

        _ => unimplemented!(),
    }
    writeln!(w, "    }}")?;
    writeln!(w, "}}\n")?;

    writeln!(w, "impl ::lutra_bin::Decode for {name} {{")?;
    writeln!(
        w,
        "    fn decode(r: &mut ::lutra_bin::Reader<'_>) -> std::io::Result<Self> {{"
    )?;
    match &ty.kind {
        pr::TyKind::Primitive(_) | pr::TyKind::Array(_) => {
            writeln!(w, "        self.0.decode(r)")?;
        }

        pr::TyKind::Tuple(fields) => {
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);
                let field_ty = &field.ty;

                write!(w, "        let {field_name} = ")?;

                let mut ctx = Context::default();
                write_ty_ref(w, field_ty, true, &mut ctx)?;

                writeln!(w, "::decode(r)?;")?;
            }

            writeln!(w, "        Ok({name} {{")?;
            for (index, field) in fields.iter().enumerate() {
                let field_name = tuple_field_name(&field.name, index);
                writeln!(w, "            {field_name},")?;
            }
            writeln!(w, "        }})")?;
        }

        _ => unimplemented!(),
    }
    writeln!(w, "    }}")?;
    writeln!(w, "}}\n")?;

    Ok(())
}
