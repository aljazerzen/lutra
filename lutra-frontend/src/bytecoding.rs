use lutra_bin::br::*;
use lutra_bin::ir;

pub fn compile_program(value: ir::Program) -> Program {
    Program {
        input_tys: value
            .get_input_tys()
            .iter()
            .map(|x| compile_ty(x.clone()))
            .collect(),
        output_ty: compile_ty(value.get_output_ty().clone()),

        externals: value
            .externals
            .into_iter()
            .map(compile_external_symbol)
            .collect(),
        main: compile_expr(value.main),
    }
}

fn compile_external_symbol(value: ir::ExternalSymbol) -> ExternalSymbol {
    let layout_args: Vec<u32> = match value.id.as_str() {
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
        id: value.id,
        layout_args,
    }
}

fn compile_expr(expr: ir::Expr) -> Expr {
    let kind = match expr.kind {
        ir::ExprKind::Pointer(v) => ExprKind::Pointer(compile_sid(v)),
        ir::ExprKind::Literal(v) => ExprKind::Literal(compile_literal(v)),
        ir::ExprKind::Call(v) => ExprKind::Call(Box::new(compile_call(*v))),
        ir::ExprKind::Function(v) => ExprKind::Function(Box::new(compile_function(*v))),
        ir::ExprKind::Tuple(v) => ExprKind::Tuple(Box::new(compile_tuple(v))),
        ir::ExprKind::Array(v) => ExprKind::Array(Box::new(compile_array(expr.ty, v))),
        ir::ExprKind::TupleLookup(v) => ExprKind::TupleLookup(Box::new(compile_tuple_lookup(*v))),
        ir::ExprKind::Binding(v) => ExprKind::Binding(Box::new(compile_binding(*v))),
    };

    Expr { kind }
}

fn compile_sid(value: ir::Sid) -> ir::Sid {
    value
}

fn compile_literal(value: ir::Literal) -> ir::Literal {
    value
}

fn compile_call(value: ir::Call) -> Call {
    Call {
        function: compile_expr(value.function),
        args: value.args.into_iter().map(compile_expr).collect(),
    }
}

fn compile_function(value: ir::Function) -> Function {
    Function {
        symbol_ns: compile_sid(value.symbol_ns),
        body: compile_expr(value.body),
    }
}

fn compile_tuple(fields: Vec<ir::Expr>) -> Tuple {
    let field_layouts = fields
        .iter()
        .map(|f| compile_ty_layout(f.ty.layout.clone().unwrap()))
        .collect();
    let fields = fields.into_iter().map(compile_expr).collect();
    Tuple {
        fields,
        field_layouts,
    }
}

fn compile_array(ty: ir::Ty, items: Vec<ir::Expr>) -> Array {
    Array {
        items: items.into_iter().map(compile_expr).collect(),
        item_layout: compile_ty_layout(ty.kind.into_array().unwrap().layout.unwrap()),
    }
}

fn compile_tuple_lookup(value: ir::TupleLookup) -> TupleLookup {
    let offset = lutra_bin::layout::tuple_field_offset(&value.base.ty, value.position);

    TupleLookup {
        base: compile_expr(value.base),
        offset,
    }
}

fn compile_binding(value: ir::Binding) -> Binding {
    Binding {
        symbol: compile_sid(value.symbol),
        expr: compile_expr(value.expr),
        main: compile_expr(value.main),
    }
}

fn compile_ty(value: ir::Ty) -> ir::Ty {
    value
}

fn compile_ty_layout(value: ir::TyLayout) -> TyLayout {
    TyLayout {
        head_size: value.head_size,
        body_ptrs: value.body_ptrs,
    }
}
