use std::borrow::Cow;
use std::collections::HashMap;
use std::ops::{BitAnd, Shr};
use std::rc::Rc;

use lutra_bin::br;
use lutra_bin::bytes;
use lutra_bin::{Data, Encode};

use crate::NativeModule;

type Addr = usize;

pub struct Interpreter {
    memory: Vec<Cell>,

    bindings: HashMap<br::Sid, Addr>,
    scopes: HashMap<br::Sid, Vec<Addr>>,
}

#[derive(Clone)]
pub enum Cell {
    Data(Data),
    Function(Box<br::Function>),
    FunctionNative(Box<(NativeFunction, Rc<[u32]>)>),
    Vacant,
}

pub type NativeFunction = &'static dyn Fn(&mut Interpreter, &[u32], Vec<Cell>) -> Cell;

pub fn evaluate(
    program: &br::Program,
    inputs: Vec<Vec<u8>>,
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
        interpreter.allocate(Cell::FunctionNative(Box::new((
            function,
            Rc::from(external.layout_args.clone()),
        ))));
    }

    // the main function call
    let main = interpreter.evaluate_expr(&program.main);
    let args = inputs
        .into_iter()
        .map(|a| Cell::Data(Data::new(a)))
        .collect();

    let res = interpreter.evaluate_func_call(&main, args);

    // extract result
    drop(interpreter);
    let Cell::Data(value) = res else { panic!() };
    value.flatten()
}

impl Interpreter {
    fn get_cell(&self, sid: br::Sid) -> &Cell {
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

    fn resolve_sid_addr(&self, sid: br::Sid) -> Addr {
        match sid.kind() {
            br::SidKind::External => {
                // externals: laid out at the start of memory
                sid.0 as Addr
            }
            br::SidKind::Var => {
                // bindings
                *self.bindings.get(&sid).unwrap()
            }
            br::SidKind::FunctionScope => {
                // function scopes
                let scope_id = br::Sid(sid.0.bitand(0xffffff00_u32));
                let start = self.scopes.get(&scope_id).and_then(|s| s.last()).unwrap();

                *start + (sid.0.bitand(0x000000ff_u32) as usize)
            }
        }
    }

    fn allocate_binding(&mut self, binding_id: br::Sid, symbol: Cell) -> Addr {
        assert_eq!(binding_id.0.shr(30), 0x1_u32);

        let addr = self.allocate(symbol);
        self.bindings.insert(binding_id, addr);

        addr
    }

    fn drop_binding(&mut self, binding_id: br::Sid) {
        assert_eq!(binding_id.0.shr(30), 0x1_u32);

        // TODO
        // let addr = self.bindings.remove(&binding_id).unwrap();
        // self.drop(addr, addr + 1);
    }

    fn allocate_scope(&mut self, scope_id: br::Sid, cells: Vec<Cell>) -> Addr {
        assert_eq!(scope_id.0.shr(30), 0x2_u32);

        let mem_start = self.memory.len() as Addr;
        self.scopes.entry(scope_id).or_default().push(mem_start);

        for cell in cells {
            self.allocate(cell);
        }
        mem_start
    }

    fn drop_scope(&mut self, scope_id: br::Sid, scope_size: usize) {
        assert_eq!(scope_id.0.shr(30), 0x2_u32);

        let sid_start = self.scopes.get_mut(&scope_id).unwrap().pop().unwrap();
        let sid_end = sid_start + scope_size;

        self.drop(sid_start, sid_end);
    }

    /// Contract:
    /// - expressions of types int, float, bool, text, tuple & array are evaluated to Symbol::Value,
    /// - expressions of func type are evaluated to Symbol::Function or Symbol::External(Function)
    fn evaluate_expr(&mut self, expr: &br::Expr) -> Cell {
        match &expr.kind {
            br::ExprKind::Pointer(sid) => {
                let mem_cell = self.get_cell(*sid);
                match mem_cell {
                    Cell::Data(..) | Cell::Function(..) | Cell::FunctionNative(..) => {
                        mem_cell.clone()
                    }

                    Cell::Vacant => panic!(),
                }
            }

            br::ExprKind::Literal(l) => {
                let mut buf = bytes::BytesMut::with_capacity(8);
                match l {
                    br::Literal::Int(i) => i.encode(&mut buf),
                    br::Literal::Float(i) => i.encode(&mut buf),
                    br::Literal::Bool(i) => i.encode(&mut buf),
                    br::Literal::Text(i) => i.encode(&mut buf),
                };
                Cell::Data(Data::new(buf.to_vec()))
            }

            br::ExprKind::Tuple(tuple) => {
                let field_layouts: Cow<_> = tuple
                    .field_layouts
                    .iter()
                    .map(|x| (x.head_size.div_ceil(8), x.body_ptrs.as_slice()))
                    .collect();
                let mut writer = lutra_bin::TupleWriter::new(field_layouts);
                for field in &tuple.fields {
                    let cell = self.evaluate_expr(field);
                    let data = cell.into_data().unwrap_or_else(|_| panic!());

                    writer.write_field(data);
                }

                Cell::Data(writer.finish())
            }
            br::ExprKind::Array(array) => {
                let mut writer = lutra_bin::ArrayWriter::new(
                    array.item_layout.head_size.div_ceil(8),
                    &array.item_layout.body_ptrs,
                );
                for item in &array.items {
                    let cell = self.evaluate_expr(item);

                    writer.write_item(cell.into_data().unwrap_or_else(|_| panic!()));
                }

                Cell::Data(writer.finish())
            }
            br::ExprKind::EnumVariant(variant) => {
                let inner = self.evaluate_expr(&variant.inner);
                let inner = inner.into_data().unwrap_or_else(|_| panic!());

                Cell::Data(lutra_bin::EnumWriter::write_variant(
                    &variant.tag,
                    variant.has_ptr,
                    variant.padding_bytes,
                    inner,
                ))
            }
            br::ExprKind::TupleLookup(lookup) => {
                let br::TupleLookup { base, offset } = lookup.as_ref();

                let base = self.evaluate_expr(base);

                let mut data = base.into_data().unwrap_or_else(|_| panic!());
                data.skip(*offset as usize);
                Cell::Data(data)
            }
            br::ExprKind::Call(call) => {
                let br::Call { function, args, .. } = call.as_ref();

                let function = self.evaluate_expr(function);

                let mut arg_cells = Vec::with_capacity(args.len());
                for arg in args {
                    let value = self.evaluate_expr(arg);
                    arg_cells.push(value);
                }
                self.evaluate_func_call(&function, arg_cells)
            }
            br::ExprKind::Function(func) => Cell::Function(func.clone()),
            br::ExprKind::Binding(binding) => {
                let br::Binding { symbol, expr, main } = binding.as_ref();

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
            Cell::FunctionNative(func) => func.0(self, &func.1, args),
            Cell::Data(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }
}

impl Cell {
    pub fn as_data(&self) -> Option<&Data> {
        match self {
            Cell::Data(val) => Some(val),
            Cell::Function(..) => None,
            Cell::FunctionNative(..) => None,
            Cell::Vacant => None,
        }
    }

    pub fn into_data(self) -> Result<Data, Cell> {
        match self {
            Cell::Data(val) => Ok(val),
            Cell::Function(..) => Err(self),
            Cell::FunctionNative(..) => Err(self),
            Cell::Vacant => Err(self),
        }
    }
}
