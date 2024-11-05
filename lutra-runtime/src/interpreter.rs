use std::collections::HashMap;
use std::ops::{BitAnd, Shr};
use std::rc::Rc;

use crate::ir;

struct Interpreter {
    symbol_table: Vec<Symbol>,
    bindings: HashMap<u32, u32>,
    scopes: HashMap<u32, Vec<u32>>,
}

#[derive(Clone)]
pub(crate) enum Symbol {
    Value(Rc<lutra_bin::Value>),
    Function(Box<ir::Function>),
    FunctionNative(NativeFunction),
    Vacant,
}

pub type NativeFunction = &'static dyn Fn(Vec<Symbol>) -> Symbol;

pub fn evaluate(program: &ir::Program, _input: ()) -> lutra_bin::Value {
    let mut interpreter = Interpreter {
        symbol_table: Vec::<Symbol>::new(),
        bindings: HashMap::new(),
        scopes: HashMap::new(),
    };

    // load external symbols
    for external in &program.externals {
        interpreter.allocate(crate::native::lookup_native_symbol(&external.id));
    }

    // evaluate the expression
    let main = interpreter.evaluate_expr(&program.main);

    // extract result
    drop(interpreter);
    let Symbol::Value(value) = main else { panic!() };
    Rc::into_inner(value).unwrap()
}

impl Interpreter {
    fn get_symbol(&self, sid: ir::Sid) -> &Symbol {
        self.symbol_table.get(self.lookup_sid(sid)).unwrap()
    }

    fn allocate(&mut self, symbol: Symbol) -> ir::Sid {
        let sid = self.symbol_table.len() as ir::Sid;
        self.symbol_table.push(symbol);
        sid
    }

    fn drop(&mut self, sid_start: usize, sid_end: usize) {
        // deallocate scope symbols
        if sid_end == self.symbol_table.len() {
            self.symbol_table.drain(sid_start..);
        } else {
            for index in sid_start..sid_end {
                let symbol = self.symbol_table.get_mut(index).unwrap();
                *symbol = Symbol::Vacant;
            }
        }
    }

    fn lookup_sid(&self, mut sid: ir::Sid) -> usize {
        let ptr_kind: u32 = sid.shr(30);

        match ptr_kind {
            0 => {
                // externals: no mapping
            }
            1 => {
                // bindings
                sid = *self.bindings.get(&sid).unwrap();
            }
            2 => {
                // function scopes
                let scope_id = sid.bitand(0xffffff00_u32);
                if let Some(start) = self.scopes.get(&scope_id).and_then(|s| s.last()) {
                    sid = *start + sid.bitand(0x000000ff_u32);
                }
            }
            _ => {
                panic!()
            }
        }

        sid as usize
    }

    fn allocate_binding(&mut self, binding_id: ir::Sid, symbol: Symbol) -> ir::Sid {
        assert_eq!(binding_id.shr(30), 0x1_u32);

        let sid = self.allocate(symbol);
        self.bindings.insert(binding_id, sid);

        sid
    }

    fn drop_binding(&mut self, binding_id: ir::Sid) {
        assert_eq!(binding_id.shr(30), 0x1_u32);

        let sid = self.bindings.remove(&binding_id).unwrap() as usize;
        self.drop(sid, sid + 1);
    }

    fn allocate_scope(&mut self, scope_id: ir::Sid, symbols: Vec<Symbol>) -> ir::Sid {
        assert_eq!(scope_id.shr(30), 0x2_u32);

        let sid_start = self.symbol_table.len() as ir::Sid;
        self.scopes.entry(scope_id).or_default().push(sid_start);

        for symbol in symbols {
            self.allocate(symbol);
        }
        sid_start
    }

    fn drop_scope(&mut self, scope_id: ir::Sid, scope_size: usize) {
        assert_eq!(scope_id.shr(30), 0x2_u32);

        let sid_start = self.scopes.get_mut(&scope_id).unwrap().pop().unwrap() as usize;
        let sid_end = sid_start + scope_size;

        self.drop(sid_start, sid_end);
    }

    /// Contract:
    /// - expressions of types int, float, bool, text, tuple & array are evaluated to Symbol::Value,
    /// - expressions of func type are evaluated to Symbol::Function or Symbol::External(Function)
    fn evaluate_expr(&mut self, expr: &ir::Expr) -> Symbol {
        match &expr.kind {
            ir::ExprKind::Pointer(sid) => {
                let symbol = self.get_symbol(*sid);
                match symbol {
                    Symbol::Value(_) | Symbol::Function(_) | Symbol::FunctionNative(_) => {
                        symbol.clone()
                    }

                    Symbol::Vacant => panic!(),
                }
            }

            ir::ExprKind::Literal(l) => Symbol::Value(Rc::new(match l {
                ir::Literal::Int(i) => lutra_bin::Value::Int(*i),
                ir::Literal::Float(i) => lutra_bin::Value::Float(*i),
                ir::Literal::Bool(i) => lutra_bin::Value::Bool(*i),
                ir::Literal::Text(i) => lutra_bin::Value::Text(i.clone()),
            })),

            ir::ExprKind::Tuple(fields) => {
                let mut res = Vec::new();
                for field in fields {
                    let symbol = self.evaluate_expr(field);
                    let value = assume_value(&symbol);
                    res.push(value);
                }
                Symbol::Value(Rc::new(lutra_bin::Value::Tuple(res)))
            }
            ir::ExprKind::Array(items) => {
                let mut res = Vec::new();
                for item in items {
                    let symbol = self.evaluate_expr(item);
                    let value = assume_value(&symbol);
                    res.push(value);
                }
                Symbol::Value(Rc::new(lutra_bin::Value::Array(res)))
            }
            ir::ExprKind::TupleLookup(ir::TupleLookup { base, offset }) => {
                let base = self.evaluate_expr(base);

                match base {
                    Symbol::Value(value) => {
                        let lutra_bin::Value::Tuple(fields) = value.as_ref() else {
                            panic!()
                        };

                        let index = *offset as usize;
                        let value = fields.get(index).unwrap_or_else(|| panic!());
                        Symbol::Value(Rc::new(value.clone()))
                    }
                    Symbol::Function(_) => panic!(),
                    Symbol::FunctionNative(_) => panic!(),
                    Symbol::Vacant => panic!(),
                }
            }
            ir::ExprKind::ArrayLookup(ir::ArrayLookup { base, offset }) => {
                let base = self.evaluate_expr(base);

                match base {
                    Symbol::Value(value) => {
                        let lutra_bin::Value::Array(items) = value.as_ref() else {
                            panic!()
                        };

                        let index = *offset as usize;
                        let value = items.get(index).unwrap_or_else(|| panic!());
                        Symbol::Value(Rc::new(value.clone()))
                    }
                    Symbol::Function(_) => panic!(),
                    Symbol::FunctionNative(_) => panic!(),
                    Symbol::Vacant => panic!(),
                }
            }

            ir::ExprKind::Call(ir::Call { function, args }) => {
                let function = self.evaluate_expr(function);

                let mut arg_symbols = Vec::new();
                for arg in args {
                    arg_symbols.push(self.evaluate_expr(arg));
                }

                let res = match function {
                    Symbol::Function(func) => {
                        let scope_size = arg_symbols.len();
                        self.allocate_scope(func.symbol_ns, arg_symbols);

                        let res = self.evaluate_expr(&func.body);

                        self.drop_scope(func.symbol_ns, scope_size);
                        res
                    }
                    Symbol::FunctionNative(native) => native(arg_symbols),
                    Symbol::Value(_) => panic!(),
                    Symbol::Vacant => panic!(),
                };

                res
            }
            ir::ExprKind::Function(func) => Symbol::Function(Box::new(func.clone())),
            ir::ExprKind::Binding(ir::Binding { symbol, expr, main }) => {
                let expr = self.evaluate_expr(expr);
                self.allocate_binding(*symbol, expr);

                let main = self.evaluate_expr(main);
                self.drop_binding(*symbol);
                main
            }
        }
    }
}

fn assume_value(symbol: &Symbol) -> lutra_bin::Value {
    match symbol {
        Symbol::Value(val) => val.as_ref().clone(),
        Symbol::Function(_) => panic!(),
        Symbol::FunctionNative(_) => panic!(),
        Symbol::Vacant => panic!(),
    }
}
