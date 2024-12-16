use std::fmt::Write;

use lutra_frontend::pr;

use crate::codegen_ty::Context;

use super::codegen_ty;

#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
pub fn write_functions(
    w: &mut impl Write,
    functions: &[(&String, &pr::TyFunc)],
    ctx: &mut Context,
) -> Result<(), std::fmt::Error> {
    if functions.is_empty() {
        return Ok(());
    }

    writeln!(w, "pub trait Functions {{")?;
    for (name, func) in functions {
        writeln!(w, "    fn {name}(")?;
        for (param_i, param) in func.params.iter().enumerate() {
            let param = param.as_ref().unwrap();
            let param = lutra_bin::ir::Ty::from(param.clone());

            write!(w, "        arg{param_i}: ")?;
            codegen_ty::write_ty_ref(w, &param, false, ctx)?;
            writeln!(w, ",")?;
        }

        let body = func.body.as_ref().unwrap().as_ref();
        let body = lutra_bin::ir::Ty::from(body.clone());
        writeln!(w, "    ) -> ")?;
        codegen_ty::write_ty_ref(w, &body, false, ctx)?;
        writeln!(w, ";")?;
    }
    writeln!(w, "}}\n")?;

    writeln!(w, "pub trait NativeFunctions {{")?;
    for (name, _func) in functions {
        writeln!(w, "    fn {name}(")?;
        writeln!(w, "        interpreter: &mut ::lutra_runtime::Interpreter,")?;
        writeln!(w, "        layout_args: &[u32],")?;
        writeln!(w, "        args: Vec<(&::lutra_bin::ir::Ty, ::lutra_runtime::Cell)>,")?;
        writeln!(w, "    ) -> ::lutra_runtime::Cell;")?;
    }
    writeln!(w, "}}\n")?;

    writeln!(w, "impl <T: Functions> NativeFunctions for T {{")?;
    for (name, func) in functions {
        writeln!(w, "    fn {name}(")?;
        writeln!(w, "        _interpreter: &mut ::lutra_runtime::Interpreter,")?;
        writeln!(w, "        _layout_args: &[u32],")?;
        writeln!(w, "        args: Vec<(&::lutra_bin::ir::Ty, ::lutra_runtime::Cell)>,")?;
        writeln!(w, "    ) -> ::lutra_runtime::Cell {{")?;
        writeln!(w, "        use lutra_bin::{{Encode, Decode}};")?;

        // decode args
        writeln!(w, "        let mut args = args.into_iter();")?;
        writeln!(w)?;
        for (param_i, param) in func.params.iter().enumerate() {
            let param = param.as_ref().unwrap();
            let param = lutra_bin::ir::Ty::from(param.clone());

            writeln!(w, "        let (_, arg{param_i}) = args.next().unwrap();")?;
            writeln!(w, "        let arg{param_i} = arg{param_i}.into_data().unwrap_or_else(|_| panic!());")?;
            write!(w, "        let arg{param_i} = ")?;
            codegen_ty::write_ty_ref(w, &param, true, ctx)?;
            writeln!(w, "::decode_buffer(&arg{param_i}.flatten()).unwrap();")?;
            writeln!(w)?;
        }

        // call
        writeln!(w, "        let res = <T as Functions>::{name}(")?;
        for (param_i, _param) in func.params.iter().enumerate() {
            writeln!(w, "            arg{param_i},")?;
        }
        writeln!(w, "        );")?;

        // encode result
        writeln!(w, "        let mut buf = Vec::new();")?;
        writeln!(w, "        res.encode(&mut buf).unwrap();")?;
        writeln!(w, "        ::lutra_runtime::Cell::Data(::lutra_bin::Data::new(buf))")?;
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
