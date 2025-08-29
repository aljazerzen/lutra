use std::fmt::Write;

use lutra_bin::ir;

use crate::{Context, codegen_ty};

#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
pub fn write_functions(
    w: &mut impl Write,
    functions: &[(&String, ir::TyFunction)],
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    if functions.is_empty() {
        return Ok(());
    }
    let lutra_bin = &ctx.options.lutra_bin_path;

    writeln!(w, "pub trait Functions {{")?;
    for (name, func) in functions {
        writeln!(w, "    fn {name}(")?;
        for (param_i, param) in func.params.iter().enumerate() {
            write!(w, "        arg{param_i}: ")?;
            codegen_ty::write_ty_ref(w, param, false, ctx)?;
            writeln!(w, ",")?;
        }

        let body = &func.body;
        write!(w, "    ) -> ")?;
        codegen_ty::write_ty_ref(w, body, false, ctx)?;
        writeln!(w, ";")?;
    }
    writeln!(w, "}}\n")?;

    writeln!(w, "pub trait NativeFunctions {{")?;
    for (name, _func) in functions {
        writeln!(w, "    fn {name}(")?;
        writeln!(w, "        interpreter: &mut ::lutra_interpreter::Interpreter,")?;
        writeln!(w, "        layout_args: &[u32],")?;
        writeln!(w, "        args: Vec<::lutra_interpreter::Cell>,")?;
        writeln!(w, "    ) -> Result<::lutra_interpreter::Cell, ::lutra_interpreter::EvalError>;")?;
    }
    writeln!(w, "}}\n")?;

    writeln!(w, "impl <T: Functions> NativeFunctions for T {{")?;
    for (name, func) in functions {
        let args = if func.params.is_empty() {
            "_args"
        } else {
            "args"
        };

        writeln!(w, "    fn {name}(")?;
        writeln!(w, "        _interpreter: &mut ::lutra_interpreter::Interpreter,")?;
        writeln!(w, "        _layout_args: &[u32],")?;
        writeln!(w, "        {args}: Vec<::lutra_interpreter::Cell>,")?;
        writeln!(w, "    ) -> Result<::lutra_interpreter::Cell, ::lutra_interpreter::EvalError> {{")?;

        if !func.params.is_empty() {
            writeln!(w, "        use {lutra_bin}::Decode;", )?;

            // decode args
            writeln!(w, "        let mut args = args.into_iter();")?;
            writeln!(w)?;
            for (param_i, param) in func.params.iter().enumerate() {
                writeln!(w, "        let arg{param_i} = args.next().unwrap();")?;
                writeln!(w, "        let arg{param_i} = arg{param_i}.into_data().unwrap_or_else(|_| panic!());")?;
                write!(w, "        let arg{param_i} = ")?;
                codegen_ty::write_ty_ref(w, param, true, ctx)?;
                writeln!(w, "::decode(&arg{param_i}.flatten()).unwrap();")?;
                writeln!(w)?;
            }
        } else {
            writeln!(w, "        use {lutra_bin}::Encode;", )?;
        }

        // call
        writeln!(w, "        let res = <T as Functions>::{name}(")?;
        for (param_i, _param) in func.params.iter().enumerate() {
            writeln!(w, "            arg{param_i},")?;
        }
        writeln!(w, "        );")?;

        // encode result
        writeln!(w, "        let buf = {lutra_bin}::Encode::encode(&res);")?;
        writeln!(w, "        Ok(::lutra_interpreter::Cell::Data(::lutra_interpreter::Data::new(buf.to_vec())))")?;
        writeln!(w, "    }}")?;
    }
    writeln!(w, "}}\n")?;

    writeln!(w, "pub struct Wrapper<T>(pub T);\n")?;

    writeln!(w, "impl <T: NativeFunctions + 'static + Sync> ::lutra_interpreter::NativeModule for Wrapper<T> {{")?;
    writeln!(w, "    fn lookup_native_symbol(&self, id: &str) -> Option<::lutra_interpreter::NativeFunction> {{")?;
    writeln!(w, "        match id {{")?;
    for (name, _func) in functions {
        writeln!(w, "          \"{name}\" => Some(&T::{name}),")?;
    }
    writeln!(w, "          _ => None,")?;
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;
    writeln!(w, "}}\n")?;

    Ok(())
}
