use std::collections::HashMap;
use std::ops::{BitAnd, Shr};
use std::rc::Rc;

use lutra_bin::Decode;
use lutra_bin::{ReaderExt, br};

use crate::{ArrayWriter, Data, EnumWriter, NativeModule};

type Addr = usize;

pub struct Interpreter {
    memory: Vec<Cell>,

    bindings: HashMap<br::Sid, Addr>,
    scopes: HashMap<br::Sid, Vec<Addr>>,

    pub(crate) file_system: Option<std::path::PathBuf>,
}

#[derive(Clone)]
pub enum Cell {
    Data(Data),
    Function(Box<br::Function>),
    FunctionNative(Box<(NativeFunction, Rc<[u32]>)>),
    Vacant,
}

pub type NativeFunction =
    &'static dyn Fn(&mut Interpreter, &[u32], Vec<Cell>) -> Result<Cell, EvalError>;

#[derive(Debug, Clone, thiserror::Error)]
pub enum EvalError {
    #[error("bad program")]
    BadProgram,
    #[error("bad inputs")]
    BadInputs,
    #[error("missing function implementation: {external_id}")]
    MissingFuncImpl { external_id: String },
    #[error("work in progress")]
    WorkInProgress,
    #[error("bug")]
    Bug,
    #[error("error: {0}")]
    ExternalError(String),
}

pub fn evaluate(
    program: &br::Program,
    input: Vec<u8>,
    native_modules: &[(&str, &dyn NativeModule)],
    file_system: Option<std::path::PathBuf>,
) -> Result<Vec<u8>, EvalError> {
    let mut interpreter = Interpreter {
        memory: Vec::<Cell>::new(),
        bindings: HashMap::new(),
        scopes: HashMap::new(),

        file_system,
    };

    // load external symbols
    let native_modules: HashMap<&str, _> = native_modules.iter().map(|a| (a.0, a.1)).collect();
    for external in &program.externals {
        let Some(function) = find_external_func(external, &native_modules) else {
            return Err(EvalError::MissingFuncImpl {
                external_id: external.id.clone(),
            });
        };
        interpreter.allocate(Cell::FunctionNative(Box::new((
            function,
            Rc::from(external.layout_args.clone()),
        ))));
    }

    // the main function call
    let main = interpreter.evaluate_expr(&program.main)?;
    let args = Cell::Data(Data::new(input));

    let res = interpreter.evaluate_func_call(&main, vec![args])?;

    // extract result
    drop(interpreter);
    let Cell::Data(value) = res else {
        return Err(EvalError::BadProgram);
    };
    Ok(value.flatten())
}

fn find_external_func(
    external: &br::ExternalSymbol,
    native_modules: &HashMap<&str, &dyn NativeModule>,
) -> Option<NativeFunction> {
    let (mod_id, def_name) = external.id.rsplit_once("::")?;
    let module = native_modules.get(mod_id)?;
    module.lookup_native_symbol(def_name)
}

impl Interpreter {
    fn get_cell(&self, sid: br::Sid) -> Result<&Cell, EvalError> {
        let addr = self.resolve_sid_addr(sid)?;
        self.memory.get(addr).ok_or(EvalError::BadProgram)
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

    fn resolve_sid_addr(&self, sid: br::Sid) -> Result<Addr, EvalError> {
        Ok(match sid.kind() {
            br::SidKind::External => {
                // externals: laid out at the start of memory
                sid.0 as Addr
            }
            br::SidKind::Var => {
                // bindings
                *self.bindings.get(&sid).ok_or(EvalError::BadProgram)?
            }
            br::SidKind::FunctionScope => {
                // function scopes
                let scope_id = br::Sid(sid.0.bitand(0xffffff00_u32));
                let start = self.scopes.get(&scope_id).and_then(|s| s.last()).unwrap();

                *start + (sid.0.bitand(0x000000ff_u32) as usize)
            }
        })
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
    fn evaluate_expr(&mut self, expr: &br::Expr) -> Result<Cell, EvalError> {
        Ok(match &expr.kind {
            br::ExprKind::Pointer(sid) => {
                let mem_cell = self.get_cell(*sid)?;
                match mem_cell {
                    Cell::Data(..) | Cell::Function(..) | Cell::FunctionNative(..) => {
                        mem_cell.clone()
                    }

                    Cell::Vacant => return Err(EvalError::Bug),
                }
            }

            br::ExprKind::Literal(data) => Cell::Data(Data::new(data.clone())),

            br::ExprKind::Tuple(tuple) => {
                let field_layouts: Vec<_> = tuple
                    .field_layouts
                    .iter()
                    .map(|x| (x.head_size.div_ceil(8), x.body_ptrs.as_slice()))
                    .collect();

                let mut field_index = 0;
                let mut writer = crate::TupleWriter::new(field_layouts.as_slice().into());
                for field in &tuple.fields {
                    let cell = self.evaluate_expr(&field.expr)?;
                    let data = cell.into_data().map_err(|_| EvalError::BadProgram)?;

                    if field.unpack == 0 {
                        // general case
                        writer.write_field(data);
                        field_index += 1;
                    } else {
                        // unpack
                        let mut offsets = Vec::with_capacity(field.unpack as usize);
                        let mut o = 0;
                        for (head_bytes, _) in field_layouts
                            .iter()
                            .skip(field_index)
                            .take(field.unpack as usize)
                        {
                            offsets.push(o);
                            o += head_bytes;
                        }
                        let reader = lutra_bin::TupleReader::new(data, offsets.into());
                        for i in 0..field.unpack {
                            writer.write_field(reader.get_field(i as usize));
                        }
                        field_index += field.unpack as usize;
                    }
                }

                Cell::Data(writer.finish())
            }
            br::ExprKind::Array(array) => {
                let mut writer = ArrayWriter::new(
                    array.item_layout.head_size.div_ceil(8),
                    &array.item_layout.body_ptrs,
                );
                for item in &array.items {
                    let cell = self.evaluate_expr(item)?;

                    writer.write_item(cell.into_data().map_err(|_| EvalError::BadProgram)?);
                }

                Cell::Data(writer.finish())
            }
            br::ExprKind::EnumVariant(variant) => {
                let inner = self.evaluate_expr(&variant.inner)?;
                let inner = inner.into_data().map_err(|_| EvalError::BadProgram)?;

                Cell::Data(EnumWriter::write_variant(
                    &variant.tag,
                    variant.inner_bytes,
                    variant.has_ptr,
                    variant.padding_bytes,
                    inner,
                ))
            }
            br::ExprKind::EnumEq(enum_eq) => {
                let expr = self.evaluate_expr(&enum_eq.expr)?;
                let expr = expr.into_data().map_err(|_| EvalError::BadProgram)?;

                let expr_tag = &expr.chunk()[0..enum_eq.tag.len()];
                let eq = expr_tag == enum_eq.tag;

                Cell::Data(Data::new(vec![if eq { 1_u8 } else { 0_u8 }]))
            }
            br::ExprKind::Offset(lookup) => {
                let base = self.evaluate_expr(&lookup.base)?;

                let mut data = base.into_data().map_err(|_| EvalError::BadProgram)?;
                data.advance(lookup.offset as usize);
                Cell::Data(data)
            }
            br::ExprKind::Deref(deref) => {
                let data = self.evaluate_expr(&deref.ptr)?;

                let mut data = data.into_data().map_err(|_| EvalError::BadProgram)?;
                let offset = u32::from_le_bytes(data.chunk().read_const());
                data.advance(offset as usize);

                Cell::Data(data)
            }
            br::ExprKind::Call(call) => {
                let br::Call { function, args, .. } = call.as_ref();

                let function = self.evaluate_expr(function)?;

                let mut arg_cells = Vec::with_capacity(args.len());
                for arg in args {
                    let value = self.evaluate_expr(arg)?;
                    arg_cells.push(value);
                }
                self.evaluate_func_call(&function, arg_cells)?
            }
            br::ExprKind::Function(func) => Cell::Function(func.clone()),
            br::ExprKind::Binding(binding) => {
                let br::Binding { symbol, expr, main } = binding.as_ref();

                let expr = self.evaluate_expr(expr)?;
                self.allocate_binding(*symbol, expr);

                let main = self.evaluate_expr(main)?;
                self.drop_binding(*symbol);
                main
            }
            br::ExprKind::Switch(branches) => {
                // At least one branch are guaranteed to match the condition.
                // This means that there will be at least one branch.
                // And that we don't have to check condition of the last branch.

                let (last, to_check) = branches.split_last().unwrap();

                for branch in to_check {
                    let condition = self.evaluate_expr(&branch.condition)?;
                    let condition = condition.into_data().map_err(|_| EvalError::BadProgram)?;
                    let condition = bool::decode(condition.chunk()).unwrap();

                    if condition {
                        return self.evaluate_expr(&branch.value);
                    }
                }
                self.evaluate_expr(&last.value)?
            }
        })
    }

    pub fn evaluate_func_call(
        &mut self,
        function: &Cell,
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        match function {
            Cell::Function(func) => {
                let scope_size = args.len();
                self.allocate_scope(func.symbol_ns, args);

                let res = self.evaluate_expr(&func.body)?;

                self.drop_scope(func.symbol_ns, scope_size);
                Ok(res)
            }
            Cell::FunctionNative(func) => func.0(self, &func.1, args),
            Cell::Data(_) => Err(EvalError::BadProgram),
            Cell::Vacant => Err(EvalError::Bug),
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
