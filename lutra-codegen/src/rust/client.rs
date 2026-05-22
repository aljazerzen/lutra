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
    let lutra_bin = &ctx.options.lutra_bin_path;
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

    for (name, func) in functions {
        let ty = compiled_program_ty(ctx, name, repr.unwrap());

        write!(w, "    pub async fn {name}(")?;
        if func.params.is_empty() {
            writeln!(w, "&self,")?;
        } else {
            writeln!(w, "&self,")?;
            write!(w, "        input: &")?;
            super::types::write_ty_ref(w, &ty.input, false, ctx)?;
            writeln!(w, ",")?;
        }
        write!(w, "    ) -> Result<{lutra_bin}::Result<")?;
        super::types::write_ty_ref(w, &ty.output, false, ctx)?;
        writeln!(w, ">, {lutra_runner}::proto::Error> {{")?;
        writeln!(w, "        let program = {name}();")?;
        if func.params.is_empty() {
            writeln!(w, "        self.runner.run(&program, &()).await")?;
        } else if func.params.len() == 1 {
            writeln!(w, "        self.runner.run(&program, input).await")?;
        } else {
            writeln!(w, "        self.runner.run(&program, input).await")?;
        }
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

    for (name, func) in functions {
        let ty = compiled_program_ty(ctx, name, repr.unwrap());

        write!(w, "    pub fn {name}(")?;
        if func.params.is_empty() {
            writeln!(w, "&mut self,")?;
        } else {
            writeln!(w, "&mut self,")?;
            write!(w, "        input: &")?;
            super::types::write_ty_ref(w, &ty.input, false, ctx)?;
            writeln!(w, ",")?;
        }
        write!(w, "    ) -> Result<{lutra_bin}::Result<")?;
        super::types::write_ty_ref(w, &ty.output, false, ctx)?;
        writeln!(w, ">, {lutra_runner}::proto::Error> {{")?;
        writeln!(w, "        let program = {name}();")?;
        if func.params.is_empty() {
            writeln!(w, "        self.runner.run_sync(&program, &())")?;
        } else {
            writeln!(w, "        self.runner.run_sync(&program, input)")?;
        }
        writeln!(w, "    }}")?;
        writeln!(w)?;
    }
    writeln!(w, "}}\n")?;

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
