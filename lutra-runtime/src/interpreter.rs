use std::collections::HashMap;
use std::ops::{BitAnd, Shr};
use std::rc::Rc;

use lutra_ir::ir;

use crate::NativeModule;

type Addr = usize;

pub struct Interpreter {
    memory: Vec<Cell>,

    bindings: HashMap<ir::Sid, Addr>,
    scopes: HashMap<ir::Sid, Vec<Addr>>,
}

#[derive(Clone)]
pub enum Cell {
    Value(Rc<lutra_bin::Value>),
    Function(Box<ir::Function>),
    FunctionNative(NativeFunction),
    Vacant,
}

pub type NativeFunction = &'static dyn Fn(&mut Interpreter, Vec<Cell>) -> lutra_bin::Value;

pub fn evaluate(
    program: &ir::Program,
    _input: (),
    native_modules: &[(&str, &dyn NativeModule)],
) -> lutra_bin::Value {
    let mut interpreter = Interpreter {
        memory: Vec::<Cell>::new(),
        bindings: HashMap::new(),
        scopes: HashMap::new(),
    };

    // load external symbols
    let native_modules: HashMap<&str, _> = native_modules.iter().map(|a| (a.0, a.1)).collect();
    for external in &program.externals {
        let (mod_id, decl_name) = external.id.rsplit_once('_').unwrap();

        let module = native_modules.get(mod_id).unwrap();
        interpreter.allocate(module.lookup_native_symbol(decl_name));
    }

    // evaluate the expression
    let main = interpreter.evaluate_expr(&program.main);

    // extract result
    drop(interpreter);
    let Cell::Value(value) = main else { panic!() };
    Rc::into_inner(value).unwrap()
}

impl Interpreter {
    fn get_cell(&self, sid: ir::Sid) -> &Cell {
        self.memory.get(self.resolve_sid_addr(sid)).unwrap()
    }

    fn allocate(&mut self, symbol: Cell) -> Addr {
        let addr = self.memory.len() as Addr;
        self.memory.push(symbol);
        addr
    }

    fn drop(&mut self, sid_start: usize, sid_end: usize) {
        // deallocate scope symbols
        if sid_end == self.memory.len() {
            self.memory.drain(sid_start..);
        } else {
            for index in sid_start..sid_end {
                let symbol = self.memory.get_mut(index).unwrap();
                *symbol = Cell::Vacant;
            }
        }
    }

    fn resolve_sid_addr(&self, sid: ir::Sid) -> Addr {
        match sid.kind() {
            ir::SidKind::External => {
                // externals: layed out at the start of memory
                sid.0 as Addr
            }
            ir::SidKind::Var => {
                // bindings
                *self.bindings.get(&sid).unwrap()
            }
            ir::SidKind::FunctionScope => {
                // function scopes
                let scope_id = ir::Sid((sid.0 as u32).bitand(0xffffff00_u32) as i64);
                let start = self.scopes.get(&scope_id).and_then(|s| s.last()).unwrap();

                *start + ((sid.0 as u32).bitand(0x000000ff_u32) as usize)
            }
        }
    }

    fn allocate_binding(&mut self, binding_id: ir::Sid, symbol: Cell) -> Addr {
        assert_eq!((binding_id.0 as u32).shr(30), 0x1_u32);

        let addr = self.allocate(symbol);
        self.bindings.insert(binding_id, addr);

        addr
    }

    fn drop_binding(&mut self, binding_id: ir::Sid) {
        assert_eq!((binding_id.0 as u32).shr(30), 0x1_u32);

        let addr = self.bindings.remove(&binding_id).unwrap();
        self.drop(addr, addr + 1);
    }

    fn allocate_scope(&mut self, scope_id: ir::Sid, symbols: Vec<Cell>) -> Addr {
        assert_eq!((scope_id.0 as u32).shr(30), 0x2_u32);

        let mem_start = self.memory.len() as Addr;
        self.scopes.entry(scope_id).or_default().push(mem_start);

        for symbol in symbols {
            self.allocate(symbol);
        }
        mem_start
    }

    fn drop_scope(&mut self, scope_id: ir::Sid, scope_size: usize) {
        assert_eq!((scope_id.0 as u32).shr(30), 0x2_u32);

        let sid_start = self.scopes.get_mut(&scope_id).unwrap().pop().unwrap();
        let sid_end = sid_start + scope_size;

        self.drop(sid_start, sid_end);
    }

    /// Contract:
    /// - expressions of types int, float, bool, text, tuple & array are evaluated to Symbol::Value,
    /// - expressions of func type are evaluated to Symbol::Function or Symbol::External(Function)
    fn evaluate_expr(&mut self, expr: &ir::Expr) -> Cell {
        match &expr.kind {
            ir::ExprKind::Pointer(sid) => {
                let mem_cell = self.get_cell(*sid);
                match mem_cell {
                    Cell::Value(_) | Cell::Function(_) | Cell::FunctionNative(_) => {
                        mem_cell.clone()
                    }

                    Cell::Vacant => panic!(),
                }
            }

            ir::ExprKind::Literal(l) => Cell::Value(Rc::new(match l {
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
                Cell::Value(Rc::new(lutra_bin::Value::Tuple(res)))
            }
            ir::ExprKind::Array(items) => {
                let mut res = Vec::new();
                for item in items {
                    let symbol = self.evaluate_expr(item);
                    let value = assume_value(&symbol);
                    res.push(value);
                }
                Cell::Value(Rc::new(lutra_bin::Value::Array(res)))
            }
            ir::ExprKind::TupleLookup(lookup) => {
                let ir::TupleLookup { base, offset } = lookup.as_ref();
                let base = self.evaluate_expr(base);

                match base {
                    Cell::Value(value) => {
                        let lutra_bin::Value::Tuple(fields) = value.as_ref() else {
                            panic!()
                        };

                        let index = *offset as usize;
                        let value = fields.get(index).unwrap_or_else(|| panic!());
                        Cell::Value(Rc::new(value.clone()))
                    }
                    Cell::Function(_) => panic!(),
                    Cell::FunctionNative(_) => panic!(),
                    Cell::Vacant => panic!(),
                }
            }
            ir::ExprKind::ArrayLookup(lookup) => {
                let ir::ArrayLookup { base, offset } = lookup.as_ref();
                let base = self.evaluate_expr(base);

                match base {
                    Cell::Value(value) => {
                        let lutra_bin::Value::Array(items) = value.as_ref() else {
                            panic!()
                        };

                        let index = *offset as usize;
                        let value = items.get(index).unwrap_or_else(|| panic!());
                        Cell::Value(Rc::new(value.clone()))
                    }
                    Cell::Function(_) => panic!(),
                    Cell::FunctionNative(_) => panic!(),
                    Cell::Vacant => panic!(),
                }
            }

            ir::ExprKind::Call(call) => {
                let ir::Call { function, args } = call.as_ref();

                let function = self.evaluate_expr(function);

                let mut arg_cells = Vec::new();
                for arg in args {
                    arg_cells.push(self.evaluate_expr(arg));
                }
                self.evaluate_func_call(&function, arg_cells)
            }
            ir::ExprKind::Function(func) => Cell::Function(func.clone()),
            ir::ExprKind::Binding(binding) => {
                let ir::Binding { symbol, expr, main } = binding.as_ref();

                let expr = self.evaluate_expr(expr);
                self.allocate_binding(*symbol, expr);

                let main = self.evaluate_expr(main);
                self.drop_binding(*symbol);
                main
            }
        }
    }

    pub fn evaluate_func_call(&mut self, function: &Cell, args: Vec<Cell>) -> Cell {
        match function {
            Cell::Function(func) => {
                let scope_size = args.len();
                self.allocate_scope(func.symbol_ns, args);

                let res = self.evaluate_expr(&func.body);

                self.drop_scope(func.symbol_ns, scope_size);
                res
            }
            Cell::FunctionNative(native) => {
                let res: lutra_bin::Value = native(self, args);
                Cell::Value(Rc::new(res))
            }
            Cell::Value(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }
}

fn assume_value(symbol: &Cell) -> lutra_bin::Value {
    match symbol {
        Cell::Value(val) => val.as_ref().clone(),
        Cell::Function(_) => panic!(),
        Cell::FunctionNative(_) => panic!(),
        Cell::Vacant => panic!(),
    }
}
