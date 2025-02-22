mod test;

use lutra_bin::ir;

pub fn print(program: &ir::Program) -> String {
    let mut printer = Printer::default();

    printer.print_program(program)
}

pub fn print_ty(ty: &ir::Ty) -> String {
    let printer = Printer::default();

    printer.print_ty(ty)
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

        r += "let main =";
        r += &self.print_expr(&program.main);
        r
    }

    fn print_expr(&mut self, expr: &ir::Expr) -> String {
        let mut r = match &expr.kind {
            ir::ExprKind::Pointer(ptr) => match ptr {
                ir::Pointer::External(ptr) => {
                    format!("external.{}", ptr.id)
                }
                ir::Pointer::Binding(id) => format!("var.{id}"),
                ir::Pointer::Parameter(ir::ParameterPtr {
                    function_id,
                    param_position,
                }) => format!("fn.{function_id}+{param_position}"),
            },
            ir::ExprKind::Literal(literal) => {
                format!("{literal}")
            }
            ir::ExprKind::Call(call) => {
                let mut r = "(".to_string();
                self.indent();
                r += &self.new_line();
                r += "call ";
                r += &self.print_expr(&call.function);
                r += ", ";
                for arg in &call.args {
                    r += &self.new_line();
                    r += &self.print_expr(arg);
                    r += ", ";
                }
                self.dedent();
                r += &self.new_line();
                r += ")";
                r
            }
            ir::ExprKind::Function(func) => {
                let mut r = "(".to_string();
                self.indent();
                r += &self.new_line();

                r += "func ";
                r += &func.id.to_string();
                r += " -> ";
                r += &self.print_expr(&func.body);

                self.dedent();
                r += &self.new_line();
                r += ")";

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
                r += &self.new_line();
                r += ".";
                r += &lookup.position.to_string();
                r
            }
            ir::ExprKind::Binding(binding) => {
                self.indent();
                let mut r = self.new_line();

                let mut binding = binding.as_ref();

                loop {
                    r += "let ";

                    r += &binding.id.to_string();

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
            ir::ExprKind::RemoteCall(call) => {
                let mut r = "(".to_string();
                self.indent();
                r += &self.new_line();
                r += "remote_call \"";
                r += &call.remote_id;
                r += "\", ";
                r += &self.new_line();
                r += &self.print_expr(&call.main);
                r += ", ";
                self.dedent();
                r += &self.new_line();
                r += ")";
                r
            }
        };

        let print_ty = !matches!(expr.kind, ir::ExprKind::Binding(_));
        if print_ty {
            r += ": ";
            r += &self.print_ty(&expr.ty);
        }
        r
    }

    #[allow(clippy::only_used_in_recursion)]
    fn print_ty(&self, ty: &ir::Ty) -> String {
        match &ty.kind {
            ir::TyKind::Primitive(ir::PrimitiveSet::bool) => "bool".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::int8) => "int8".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::int16) => "int16".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::int32) => "int32".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::int64) => "int64".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::uint8) => "uint8".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::uint16) => "uint16".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::uint32) => "uint32".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::uint64) => "uint64".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::float32) => "float32".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::float64) => "float64".to_string(),
            ir::TyKind::Primitive(ir::PrimitiveSet::text) => "text".to_string(),
            ir::TyKind::Tuple(fields) => {
                let mut r = "{".to_string();
                for (index, field) in fields.iter().enumerate() {
                    if index > 0 {
                        r += ", ";
                    }
                    if let Some(name) = &field.name {
                        r += name;
                        r += " = ";
                    }
                    r += &self.print_ty(&field.ty);
                }
                r += "}";
                r
            }
            ir::TyKind::Array(items) => {
                format!("[{}]", self.print_ty(items))
            }
            ir::TyKind::Enum(variants) => {
                let mut r = "enum {".to_string();
                for (index, variant) in variants.iter().enumerate() {
                    if index > 0 {
                        r += ", ";
                    }
                    r += &variant.name;
                    r += " = ";
                    r += &self.print_ty(&variant.ty);
                }
                r += "}";
                r
            }
            ir::TyKind::Function(func) => {
                let mut r = "func (".to_string();
                for (index, param) in func.params.iter().enumerate() {
                    if index > 0 {
                        r += ", ";
                    }
                    r += &self.print_ty(param);
                }
                r += ") -> ";
                r += &self.print_ty(&func.body);
                r
            }
            ir::TyKind::Ident(path) => {
                // TODO: quote
                path.0.join(".")
            }
        }
    }
}

#[track_caller]
pub fn _test_print(source: &str) -> String {
    let program = super::_test_parse(source);

    crate::print(&program)
}
