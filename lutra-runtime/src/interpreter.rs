use std::collections::HashMap;
use std::ops::{BitAnd, Shr};

use lutra_bin::ir;
use lutra_bin::{Data, Encode};

use crate::NativeModule;

type Addr = usize;

pub struct Interpreter {
    memory: Vec<Cell>,

    bindings: HashMap<ir::Sid, Addr>,
    scopes: HashMap<ir::Sid, Vec<Addr>>,
}

#[derive(Clone)]
pub enum Cell {
    Data(Data),
    Function(Box<ir::Function>),
    FunctionNative(NativeFunction),
    Vacant,
}

pub type NativeFunction = &'static dyn Fn(&mut Interpreter, Vec<(&ir::Ty, Cell)>) -> Cell;

pub fn evaluate(
    program: &ir::Program,
    _input: (),
    native_modules: &[(&str, &dyn NativeModule)],
) -> Vec<u8> {
    let mut interpreter = Interpreter {
        memory: Vec::<Cell>::new(),
        bindings: HashMap::new(),
        scopes: HashMap::new(),
    };

    // load external symbols
    let native_modules: HashMap<&str, _> = native_modules.iter().map(|a| (a.0, a.1)).collect();
    for external in &program.externals {
        let (mod_id, decl_name) = external
            .id
            .rsplit_once("::")
            .unwrap_or_else(|| panic!("bad external id: {}", external.id));

        let module = native_modules
            .get(mod_id)
            .unwrap_or_else(|| panic!("cannot find native module {mod_id}"));
        let function = module.lookup_native_symbol(decl_name);
        interpreter.allocate(Cell::FunctionNative(function));
    }

    // evaluate the expression
    let main = interpreter.evaluate_expr(&program.main);

    // extract result
    drop(interpreter);
    let Cell::Data(value) = main else { panic!() };
    value.flatten()
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
                let scope_id = ir::Sid(sid.0.bitand(0xffffff00_u32));
                let start = self.scopes.get(&scope_id).and_then(|s| s.last()).unwrap();

                *start + (sid.0.bitand(0x000000ff_u32) as usize)
            }
        }
    }

    fn allocate_binding(&mut self, binding_id: ir::Sid, symbol: Cell) -> Addr {
        assert_eq!(binding_id.0.shr(30), 0x1_u32);

        let addr = self.allocate(symbol);
        self.bindings.insert(binding_id, addr);

        addr
    }

    fn drop_binding(&mut self, binding_id: ir::Sid) {
        assert_eq!(binding_id.0.shr(30), 0x1_u32);

        let addr = self.bindings.remove(&binding_id).unwrap();
        self.drop(addr, addr + 1);
    }

    fn allocate_scope(&mut self, scope_id: ir::Sid, cells: Vec<(&ir::Ty, Cell)>) -> Addr {
        assert_eq!(scope_id.0.shr(30), 0x2_u32);

        let mem_start = self.memory.len() as Addr;
        self.scopes.entry(scope_id).or_default().push(mem_start);

        for (_, cell) in cells {
            self.allocate(cell);
        }
        mem_start
    }

    fn drop_scope(&mut self, scope_id: ir::Sid, scope_size: usize) {
        assert_eq!(scope_id.0.shr(30), 0x2_u32);

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
                    Cell::Data(_) | Cell::Function(_) | Cell::FunctionNative(_) => mem_cell.clone(),

                    Cell::Vacant => panic!(),
                }
            }

            ir::ExprKind::Literal(l) => {
                let mut buf = Vec::new();
                match l {
                    ir::Literal::Int(i) => i.encode(&mut buf).unwrap(),
                    ir::Literal::Float(i) => i.encode(&mut buf).unwrap(),
                    ir::Literal::Bool(i) => i.encode(&mut buf).unwrap(),
                    ir::Literal::Text(i) => i.encode(&mut buf).unwrap(),
                };
                Cell::Data(Data::new(buf))
            }

            ir::ExprKind::Tuple(fields) => {
                let mut writer = lutra_bin::TupleWriter::new(&expr.ty);
                for field in fields {
                    let cell = self.evaluate_expr(field);
                    let data = cell.into_data().unwrap_or_else(|_| panic!());

                    writer.write_field(data);
                }

                Cell::Data(writer.finish())
            }
            ir::ExprKind::Array(items) => {
                let mut writer = lutra_bin::ArrayWriter::new(&expr.ty);
                for item in items {
                    let cell = self.evaluate_expr(item);

                    writer.write_item(cell.into_data().unwrap_or_else(|_| panic!()));
                }

                Cell::Data(writer.finish())
            }
            ir::ExprKind::TupleLookup(lookup) => {
                let ir::TupleLookup { base, offset } = lookup.as_ref();
                let base_ty = &base.ty;

                let base = self.evaluate_expr(base);
                let base = base.into_data().unwrap_or_else(|_| panic!());

                let base = lutra_bin::TupleReader::new(&base, base_ty);

                Cell::Data(base.get_field(*offset as usize))
            }
            ir::ExprKind::Call(call) => {
                let ir::Call { function, args, .. } = call.as_ref();

                let function = self.evaluate_expr(function);

                let mut arg_cells = Vec::new();
                for arg in args {
                    let value = self.evaluate_expr(arg);
                    arg_cells.push((&arg.ty, value));
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

    pub fn evaluate_func_call(&mut self, function: &Cell, args: Vec<(&ir::Ty, Cell)>) -> Cell {
        match function {
            Cell::Function(func) => {
                let scope_size = args.len();
                self.allocate_scope(func.symbol_ns, args);

                let res = self.evaluate_expr(&func.body);

                self.drop_scope(func.symbol_ns, scope_size);
                res
            }
            Cell::FunctionNative(native) => native(self, args),
            Cell::Data(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }
}

impl Cell {
    pub fn as_data(&self) -> Option<&Data> {
        match self {
            Cell::Data(val) => Some(val),
            Cell::Function(_) => None,
            Cell::FunctionNative(_) => None,
            Cell::Vacant => None,
        }
    }

    pub fn into_data(self) -> Result<Data, Cell> {
        match self {
            Cell::Data(val) => Ok(val),
            Cell::Function(_) => Err(self),
            Cell::FunctionNative(_) => Err(self),
            Cell::Vacant => Err(self),
        }
    }
}
