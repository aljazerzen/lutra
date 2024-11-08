mod test;

use crate::ir;

pub fn print(program: &ir::Program) -> String {
    let mut printer = Printer::default();

    printer.print_program(program)
}

#[derive(Clone, Default)]
struct Printer {
    indent: usize,
}

const INDENT: usize = 2;

impl Printer {
    fn indent(&mut self) {
        self.indent += INDENT;
    }

    fn dedent(&mut self) {
        self.indent -= INDENT;
    }

    fn new_line(&self) -> String {
        let mut r = "\n".to_string();
        r += &" ".repeat(self.indent);
        r
    }
}

impl Printer {
    fn print_program(&mut self, program: &ir::Program) -> String {
        let mut r = String::new();
        if !program.externals.is_empty() {
            r += "let externals = [";
            self.indent();
            for external in &program.externals {
                r += &self.new_line();
                r += &format!("{},", external.id);
            }
            self.dedent();
            r += "\n];\n\n";
        }

        r += "let main =";
        r += &self.print_expr(&program.main);
        r
    }

    fn print_expr(&mut self, v: &ir::Expr) -> String {
        match &v.kind {
            ir::ExprKind::Pointer(sid) => {
                let id = (sid.0 as u32) & 0x3fffffff;
                match sid.kind() {
                    ir::SidKind::External => format!("external.{id}"),
                    ir::SidKind::Var => format!("var.{id}"),
                    ir::SidKind::FunctionScope => {
                        let func = id >> 8;
                        let param = id & 0xff;
                        format!("fn.{func}+{param}")
                    }
                }
            }
            ir::ExprKind::Literal(literal) => {
                format!("{literal}")
            }
            ir::ExprKind::Call(call) => {
                let mut r = "(".to_string();
                for (index, arg) in call.args.iter().enumerate() {
                    if index > 0 {
                        r += ", ";
                    }
                    r += &self.print_expr(arg);
                }
                r += ") | ";
                r += &self.print_expr(&call.function);
                r
            }
            ir::ExprKind::Function(func) => {
                let mut r = "func ".to_string();

                let func_id = (func.symbol_ns.0 as u32 & 0x3fffffff) >> 8;
                r += &func_id.to_string();
                r += " -> ";
                r += &self.print_expr(&func.body);

                r
            }
            ir::ExprKind::Tuple(fields) => {
                let mut r = "{".to_string();
                self.indent();
                for field in fields {
                    r += &self.new_line();
                    r += &self.print_expr(field);
                    r += ",";
                }
                self.dedent();
                r += &self.new_line();
                r += "}";
                r
            }
            ir::ExprKind::Array(items) => {
                let mut r = "[".to_string();
                self.indent();
                for item in items {
                    r += &self.new_line();
                    r += &self.print_expr(item);
                    r += ",";
                }
                self.dedent();
                r += &self.new_line();
                r += "]";
                r
            }
            ir::ExprKind::TupleLookup(lookup) => {
                let mut r = self.print_expr(&lookup.base);
                r += ".";
                r += &lookup.offset.to_string();
                r
            }
            ir::ExprKind::ArrayLookup(lookup) => {
                let mut r = self.print_expr(&lookup.base);
                r += "[";
                r += &lookup.offset.to_string();
                r += "]";
                r
            }
            ir::ExprKind::Binding(binding) => {
                self.indent();
                let mut r = self.new_line();

                let mut binding = binding.as_ref();

                loop {
                    r += "let ";

                    let id = (binding.symbol.0 as u32) & 0x3fffffff;
                    r += &id.to_string();

                    r += " = ";
                    r += &self.print_expr(&binding.expr);
                    r += ";";
                    r += &self.new_line();

                    if let ir::ExprKind::Binding(inner) = &binding.main.kind {
                        binding = inner.as_ref();
                    } else {
                        break;
                    }
                }

                r += &self.print_expr(&binding.main);
                self.dedent();
                r
            }
        }
    }
}

#[track_caller]
pub fn _test_print(source: &str) -> String {
    let program = super::_test_parse(source);

    crate::print(&program)
}
