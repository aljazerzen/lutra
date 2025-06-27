use std::fmt::Write;

use lutra_bin::{Encode, ir};
use lutra_compiler::pr;

use crate::{Context, codegen_ty};

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

    for (name, func) in functions {
        let mut fq_path = pr::Path::new(&ctx.current_rust_mod);
        fq_path.push((*name).clone());

        // compile
        let program = lutra_compiler::compile_to_sql(ctx.project, &fq_path);

        // encode and write to file
        let out_file = ctx.out_dir.join(format!("{fq_path}.sr.lb"));
        let mut buf = bytes::BytesMut::new();
        program.encode(&mut buf);
        std::fs::write(out_file, buf).unwrap();

        write!(w, "pub fn {name}() -> {lutra_bin}::sr::TypedProgram<(), ")?;
        codegen_ty::write_ty_ref(w, &func.body, false, ctx)?;
        writeln!(w, "> {{")?;
        writeln!(w, "    use {lutra_bin}::Decode;")?;
        writeln!(w, "    let program_lb = include_bytes!(\"./{fq_path}.sr.lb\");")?;
        writeln!(w, "    let program = {lutra_bin}::sr::Program::decode(program_lb).unwrap();")?;
        writeln!(w, "    {lutra_bin}::sr::TypedProgram::from(program)")?;
        writeln!(w, "}}\n")?;
    }

    Ok(())
}
