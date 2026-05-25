use std::fmt::Write;

use lutra_bin::ir;
use lutra_compiler::pr;

use crate::rust::Context;

#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
pub fn write_client(
    w: &mut impl Write,
    functions: &[(&String, ir::TyFunction)],
    sub_modules: &[&String],
    repr: Option<lutra_compiler::ProgramRepr>,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let lutra_runner = &ctx.options.lutra_runner_path;

    writeln!(w, "pub struct Client<R> {{")?;
    writeln!(w, "    pub(crate) runner: R,")?;
    writeln!(w, "}}\n")?;

    writeln!(w, "impl<'a, T: {lutra_runner}::Run + ?Sized> Client<&'a T> {{")?;
    writeln!(w, "    pub fn new(runner: &'a T) -> Self {{")?;
    writeln!(w, "        Self {{ runner }}")?;
    writeln!(w, "    }}")?;
    writeln!(w)?;

    for name in sub_modules {
        writeln!(w, "    pub fn {name}(&self) -> {name}::Client<&'a T> {{")?;
        writeln!(w, "        {name}::Client {{ runner: self.runner }}")?;
        writeln!(w, "    }}")?;
        writeln!(w)?;
    }

    for (name, _func) in functions {
        let ty = compiled_program_ty(ctx, name, repr.unwrap());

        write!(w, "    pub async fn {name}(\n        &self,\n")?;
        write_client_method_params(w, &ty.input, ctx)?;
        write_client_result_ty(w, &ty.output, ctx)?;
        writeln!(w, " {{")?;
        writeln!(w, "        let program = {name}();")?;
        write_encode_input(w, &ty.input, ctx)?;
        writeln!(w, "        let program_id = self.runner.prepare(program.inner.clone()).await?;")?;
        writeln!(w, "        let output = self.runner.execute(program_id, &input_buf).await?;")?;
        writeln!(w, "        self.runner.release(program_id).await?;")?;
        write!(w, "        Ok(<")?;
        super::types::write_ty_ref(w, &ty.output, false, ctx)?;
        writeln!(w, " as {}::Decode>::decode(&output))", ctx.options.lutra_bin_path)?;
        writeln!(w, "    }}")?;
        writeln!(w)?;
    }
    writeln!(w, "}}\n")?;

    writeln!(w, "impl<'a, T: {lutra_runner}::RunSync + ?Sized> Client<&'a mut T> {{")?;
    writeln!(w, "    pub fn new_sync(runner: &'a mut T) -> Self {{")?;
    writeln!(w, "        Self {{ runner }}")?;
    writeln!(w, "    }}")?;
    writeln!(w)?;

    for name in sub_modules {
        writeln!(w, "    pub fn {name}(&mut self) -> {name}::Client<&mut T> {{")?;
        writeln!(w, "        {name}::Client {{ runner: &mut *self.runner }}")?;
        writeln!(w, "    }}")?;
        writeln!(w)?;
    }

    for (name, _func) in functions {
        let ty = compiled_program_ty(ctx, name, repr.unwrap());

        write!(w, "    pub fn {name}(\n        &mut self,\n")?;
        write_client_method_params(w, &ty.input, ctx)?;
        write_client_result_ty(w, &ty.output, ctx)?;
        writeln!(w, " {{")?;
        writeln!(w, "        let program = {name}();")?;
        write_encode_input(w, &ty.input, ctx)?;
        writeln!(w, "        let program_id = self.runner.prepare_sync(program.inner.clone())?;")?;
        writeln!(w, "        let output = self.runner.execute_sync(program_id, &input_buf)?;")?;
        writeln!(w, "        self.runner.release_sync(program_id)?;")?;
        write!(w, "        Ok(<")?;
        super::types::write_ty_ref(w, &ty.output, false, ctx)?;
        writeln!(w, " as {}::Decode>::decode(&output))", ctx.options.lutra_bin_path)?;
        writeln!(w, "    }}")?;
        writeln!(w)?;
    }
    writeln!(w, "}}\n")?;

    Ok(())
}

fn write_client_method_params(
    w: &mut impl Write,
    input: &ir::Ty,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    if input.is_unit() {
        return Ok(());
    }

    if let ir::TyKind::Tuple(fields) = &input.kind {
        // flatten tuples
        for (index, field) in fields.iter().enumerate() {
            let field_name = crate::tuple_field_name(&field.name, index);
            write!(w, "        {field_name}: &")?;
            super::types::write_ty_ref(w, &field.ty, false, ctx)?;
            writeln!(w, ",")?;
        }
    } else {
        write!(w, "        input: &")?;
        super::types::write_ty_ref(w, input, false, ctx)?;
        writeln!(w, ",")?;
    }

    Ok(())
}

fn write_client_result_ty(
    w: &mut impl Write,
    output: &ir::Ty,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    write!(w, "    ) -> Result<{}::Result<", ctx.options.lutra_bin_path)?;
    super::types::write_ty_ref(w, output, false, ctx)?;
    write!(w, ">, {}::proto::Error>", ctx.options.lutra_runner_path)
}

#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
fn write_encode_input(
    w: &mut impl Write,
    input: &ir::Ty,
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    let lutra_bin = &ctx.options.lutra_bin_path;

    writeln!(w, "        use {lutra_bin}::Encode;")?;
    writeln!(w, "        let mut input_buf = {lutra_bin}::bytes::BytesMut::new();")?;

    if input.is_unit() {
        return Ok(());
    }

    if let ir::TyKind::Tuple(fields) = &input.kind {
        // encode tuples
        for (index, field) in fields.iter().enumerate() {
            let field_name = crate::tuple_field_name(&field.name, index);
            writeln!(w, "        let {field_name}_head = {field_name}.encode_head(&mut input_buf);")?;
        }
        for (index, field) in fields.iter().enumerate() {
            let field_name = crate::tuple_field_name(&field.name, index);
            writeln!(w, "        {field_name}.encode_body({field_name}_head, &mut input_buf);")?;
        }
    } else {
        writeln!(w, "        let input_head = input.encode_head(&mut input_buf);")?;
        writeln!(w, "        input.encode_body(input_head, &mut input_buf);")?;
    }

    Ok(())
}

fn compiled_program_ty(
    ctx: &Context,
    name: &str,
    repr: lutra_compiler::ProgramRepr,
) -> lutra_bin::rr::ProgramType {
    let mut fq_path = pr::Path::new(&ctx.current_rust_mod);
    fq_path.push(name.to_string());
    let fq_path = fq_path.to_string();

    let (_, mut ty) = lutra_compiler::compile(
        ctx.project,
        &lutra_compiler::CompileParams::new(&fq_path, repr),
    )
    .unwrap();
    crate::infer_names_of_program_ty(&mut ty, name);
    ty
}
