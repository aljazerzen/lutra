use indexmap::IndexSet;
use lutra_bin::br::*;
use lutra_bin::ir;

pub fn compile_program(value: ir::Program) -> Program {
    let mut b = ByteCoder {
        externals: Default::default(),
    };

    Program {
        input_tys: value
            .get_input_tys()
            .iter()
            .map(|x| b.compile_ty(x.clone()))
            .collect(),
        output_ty: b.compile_ty(value.get_output_ty().clone()),

        main: b.compile_expr(value.main),
        externals: b
            .externals
            .into_iter()
            .map(compile_external_symbol)
            .collect(),
    }
}

struct ByteCoder {
    externals: IndexSet<String>,
}

impl ByteCoder {
    fn compile_expr(&mut self, expr: ir::Expr) -> Expr {
        let kind = match expr.kind {
            ir::ExprKind::Pointer(v) => ExprKind::Pointer(self.compile_pointer(v)),
            ir::ExprKind::Literal(v) => ExprKind::Literal(self.compile_literal(v)),
            ir::ExprKind::Call(v) => ExprKind::Call(Box::new(self.compile_call(*v))),
            ir::ExprKind::Function(v) => ExprKind::Function(Box::new(self.compile_function(*v))),
            ir::ExprKind::Tuple(v) => ExprKind::Tuple(Box::new(self.compile_tuple(v))),
            ir::ExprKind::Array(v) => ExprKind::Array(Box::new(self.compile_array(expr.ty, v))),
            ir::ExprKind::TupleLookup(v) => {
                ExprKind::TupleLookup(Box::new(self.compile_tuple_lookup(*v)))
            }
            ir::ExprKind::Binding(v) => ExprKind::Binding(Box::new(self.compile_binding(*v))),
            ir::ExprKind::RemoteCall(_) => todo!(),
        };

        Expr { kind }
    }

    fn compile_pointer(&mut self, ptr: ir::Pointer) -> Sid {
        match ptr {
            ir::Pointer::External(ptr) => {
                let (index, _) = self.externals.insert_full(ptr.id);

                Sid(index as u32).with_tag(SidKind::External)
            }
            #[rustfmt::skip]
            ir::Pointer::Binding(binding_id) => {
                Sid(binding_id).with_tag(SidKind::Var)
            },
            ir::Pointer::Parameter(param_ptr) => {
                let sid = param_ptr.function_id << 8 | param_ptr.param_position as u32;

                Sid(sid).with_tag(SidKind::FunctionScope)
            }
        }
    }

    fn compile_literal(&mut self, value: ir::Literal) -> ir::Literal {
        value
    }

    fn compile_call(&mut self, value: ir::Call) -> Call {
        Call {
            function: self.compile_expr(value.function),
            args: value
                .args
                .into_iter()
                .map(|x| self.compile_expr(x))
                .collect(),
        }
    }

    fn compile_function(&mut self, value: ir::Function) -> Function {
        Function {
            symbol_ns: Sid(value.id << 8).with_tag(SidKind::FunctionScope),
            body: self.compile_expr(value.body),
        }
    }

    fn compile_tuple(&mut self, fields: Vec<ir::Expr>) -> Tuple {
        let field_layouts = fields
            .iter()
            .map(|f| self.compile_ty_layout(f.ty.layout.clone().unwrap()))
            .collect();
        let fields = fields.into_iter().map(|x| self.compile_expr(x)).collect();
        Tuple {
            fields,
            field_layouts,
        }
    }

    fn compile_array(&mut self, ty: ir::Ty, items: Vec<ir::Expr>) -> Array {
        Array {
            items: items.into_iter().map(|x| self.compile_expr(x)).collect(),
            item_layout: self.compile_ty_layout(ty.kind.into_array().unwrap().layout.unwrap()),
        }
    }

    fn compile_tuple_lookup(&mut self, value: ir::TupleLookup) -> TupleLookup {
        let offset = lutra_bin::layout::tuple_field_offset(&value.base.ty, value.position);

        TupleLookup {
            base: self.compile_expr(value.base),
            offset,
        }
    }

    fn compile_binding(&mut self, value: ir::Binding) -> Binding {
        Binding {
            symbol: Sid(value.id).with_tag(SidKind::Var),
            expr: self.compile_expr(value.expr),
            main: self.compile_expr(value.main),
        }
    }

    fn compile_ty(&mut self, value: ir::Ty) -> ir::Ty {
        value
    }

    fn compile_ty_layout(&mut self, value: ir::TyLayout) -> TyLayout {
        TyLayout {
            head_size: value.head_size,
            body_ptrs: value.body_ptrs,
        }
    }
}

fn compile_external_symbol(external_symbol_id: String) -> ExternalSymbol {
    let layout_args: Vec<u32> = match external_symbol_id.as_str() {
        "std::index" => vec![8],
        "std::map" => vec![8, 8, 0],
        "std::filter" => vec![
            8, // item_head_size
            0, // item_body_ptrs
        ],
        "std::slice" => vec![
            8, // item_head_size
            0, // item_body_ptrs
        ],
        "std::sort" => vec![
            8, // item_head_size
            0, // item_body_ptrs
        ],
        "std::to_columnar" => vec![
            16, // item_head_size
            2, 0, 8, // fields_offsets
            2, 8, 8, // fields_head_bytes
            0, // fields_body_ptrs[0]
            0, // fields_body_ptrs[1]
        ],
        "std::from_columnar" => vec![
            16, // output_head_bytes
            0,  // output_body_ptrs
            2, 8, 8, // fields_item_head_bytes
            0, // fields_body_ptrs[0]
            0, // fields_body_ptrs[1]
        ],

        "std::min" => vec![
            8, // item_head_size
        ],
        "std::max" => vec![
            8, // item_head_size
        ],
        "std::sum" => vec![
            8, // item_head_size
        ],
        "std::count" => vec![],
        "std::average" => vec![
            8, // item_head_size
        ],
        "std::contains" => vec![
            8, // item_head_size
        ],
        "std::lag" => vec![
            8, // item_head_size
            0, // items_body_ptrs
        ],
        "std::lead" => vec![
            8, // item_head_size
            0, // items_body_ptrs
        ],
        _ => vec![],
    };
    ExternalSymbol {
        id: external_symbol_id,
        layout_args,
    }
}
