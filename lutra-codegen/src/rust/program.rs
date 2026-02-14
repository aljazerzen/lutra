use std::fmt::Write;

use lutra_bin::{Encode, ir};
use lutra_compiler::{ProgramFormat, pr};

use crate::rust::Context;
use crate::rust::types;

#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
pub fn write_rr_programs(
    w: &mut impl Write,
    functions: &[(&String, ir::TyFunction)],
    format: ProgramFormat,
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
        let (program, mut ty) =
            lutra_compiler::compile(ctx.project, &fq_path, None, format).unwrap();

        // encode and write to file
        let out_file = ctx.out_dir.join(format!("{fq_path}.rr.lb"));
        let buf = program.encode();
        std::fs::write(out_file, buf).unwrap();

        crate::infer_names_of_program_ty(&mut ty, name);

        write!(w, "pub fn {name}() -> {lutra_bin}::rr::TypedProgram<")?;
        types::write_ty_ref(w, &ty.input, false, ctx)?;
        write!(w, ", ")?;
        types::write_ty_ref(w, &ty.output, false, ctx)?;
        writeln!(w, "> {{")?;

        writeln!(w, "    use {lutra_bin}::Decode;")?;
        writeln!(w, "    let program_lb = include_bytes!(\"./{fq_path}.rr.lb\");")?;
        writeln!(w, "    let program = {lutra_bin}::rr::Program::decode(program_lb).unwrap();")?;
        writeln!(w, "    {lutra_bin}::rr::TypedProgram::from(program)")?;
        writeln!(w, "}}\n")?;
    }

    Ok(())
}
