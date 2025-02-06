use std::fmt::Write;

use lutra_bin::ir;

use crate::{codegen_ty, Context};

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
        writeln!(w, "        interpreter: &mut ::lutra_runtime::Interpreter,")?;
        writeln!(w, "        layout_args: &[u32],")?;
        writeln!(w, "        args: Vec<::lutra_runtime::Cell>,")?;
        writeln!(w, "    ) -> ::lutra_runtime::Cell;")?;
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
        writeln!(w, "        _interpreter: &mut ::lutra_runtime::Interpreter,")?;
        writeln!(w, "        _layout_args: &[u32],")?;
        writeln!(w, "        {args}: Vec<::lutra_runtime::Cell>,")?;
        writeln!(w, "    ) -> ::lutra_runtime::Cell {{")?;

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
        writeln!(w, "        let mut buf = {lutra_bin}::bytes::BytesMut::new();")?;
        writeln!(w, "        {lutra_bin}::Encode::encode(&res, &mut buf);")?;
        writeln!(w, "        ::lutra_runtime::Cell::Data({lutra_bin}::Data::new(buf.to_vec()))")?;
        writeln!(w, "    }}")?;
    }
    writeln!(w, "}}\n")?;

    writeln!(w, "pub struct Wrapper<T>(pub T);\n")?;

    writeln!(w, "impl <T: NativeFunctions + 'static + Sync> ::lutra_runtime::NativeModule for Wrapper<T> {{")?;
    writeln!(w, "    fn lookup_native_symbol(&self, id: &str) -> ::lutra_runtime::NativeFunction {{")?;
    writeln!(w, "        match id {{")?;
    for (name, _func) in functions {
        writeln!(w, "          \"{name}\" => &T::{name},")?;
    }
    writeln!(w, "          _ => panic!(),")?;
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;
    writeln!(w, "}}\n")?;

    Ok(())
}
