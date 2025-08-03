use std::fmt::Write;

use lutra_bin::{Encode, ir};
use lutra_compiler::pr;

use crate::{Context, codegen_ty, snake_to_sentence};

#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
pub fn write_sr_programs(
    w: &mut impl Write,
    functions: &[(&String, ir::TyFunction)],
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    if functions.is_empty() {
        return Ok(());
    }
    let lutra_bin = &ctx.options.lutra_bin_path;

    for (name, _) in functions {
        let mut fq_path = pr::Path::new(&ctx.current_rust_mod);
        fq_path.push((*name).clone());
        let fq_path = fq_path.to_string();

        // compile
        let (program, mut ty) = lutra_compiler::compile(
            ctx.project,
            &fq_path,
            None,
            lutra_compiler::ProgramFormat::SqlPg,
        )
        .unwrap();

        // encode and write to file
        let out_file = ctx.out_dir.join(format!("{fq_path}.sr.lb"));
        let buf = program.encode();
        std::fs::write(out_file, buf).unwrap();

        let name_camel = snake_to_sentence(name);
        ty.input.name = Some(format!("{name_camel}Input"));
        ty.output.name = Some(format!("{name_camel}Output"));

        write!(w, "pub fn {name}() -> {lutra_bin}::rr::TypedProgram<")?;
        codegen_ty::write_ty_ref(w, &ty.input, false, ctx)?;
        write!(w, ", ")?;
        codegen_ty::write_ty_ref(w, &ty.output, false, ctx)?;
        writeln!(w, "> {{")?;

        writeln!(w, "    use {lutra_bin}::Decode;")?;
        writeln!(w, "    let program_lb = include_bytes!(\"./{fq_path}.sr.lb\");")?;
        writeln!(w, "    let program = {lutra_bin}::rr::Program::decode(program_lb).unwrap();")?;
        writeln!(w, "    {lutra_bin}::rr::TypedProgram::from(program)")?;
        writeln!(w, "}}\n")?;
    }

    Ok(())
}
