use std::fmt::Write;

use lutra_frontend::pr;

#[rustfmt::skip::macros(writeln)]
#[rustfmt::skip::macros(write)]
pub fn write_functions(
    w: &mut impl Write,
    functions: &[(&String, &pr::TyFunc)],
) -> Result<(), std::fmt::Error> {
    if functions.is_empty() {
        return Ok(());
    }

    writeln!(w, "pub trait NativeFunctions {{")?;
    for (name, _func) in functions {
        writeln!(w, "    fn {name}(interpreter: &mut ::lutra_runtime::Interpreter, args: Vec<(&::lutra_bin::ir::Ty, ::lutra_runtime::Cell)>) -> ::lutra_runtime::Cell;")?;
    }
    writeln!(w, "}}\n")?;

    writeln!(w, "pub struct Wrapper<T>(pub T);\n")?;

    writeln!(w, "impl <T: NativeFunctions + 'static + Sync> ::lutra_runtime::NativeModule for Wrapper<T> {{")?;
    writeln!(w, "    fn lookup_native_symbol(&self, id: &str) -> ::lutra_runtime::Cell {{")?;
    writeln!(w, "        match id {{")?;
    for (name, _func) in functions {
        writeln!(w, "          \"{name}\" => ::lutra_runtime::Cell::FunctionNative(&T::{name}),")?;
    }
    writeln!(w, "          _ => panic!(),")?;
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;
    writeln!(w, "}}\n")?;

    Ok(())
}
