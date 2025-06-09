use std::collections::HashMap;

use indexmap::IndexSet;
use lutra_bin::br::*;
use lutra_bin::bytes::Buf;
use lutra_bin::bytes::BufMut;
use lutra_bin::ir;
use lutra_bin::Encode;

pub fn compile_program(value: ir::Program) -> Program {
    let input_count = value.get_input_tys().len() as u8;

    let mut b = ByteCoder {
        externals: Default::default(),
        types: value
            .types
            .into_iter()
            .map(|def| (def.name, def.ty))
            .collect(),
    };

    Program {
        main: b.compile_expr(value.main),
        externals: b.externals.into_iter().collect(),
        input_count,
    }
}

struct ByteCoder {
    externals: IndexSet<ExternalSymbol>,
    types: HashMap<ir::Path, ir::Ty>,
}

impl ByteCoder {
    fn get_ty_mat<'a>(&'a self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }

    fn compile_expr(&mut self, expr: ir::Expr) -> Expr {
        let kind = match expr.kind {
            ir::ExprKind::Pointer(v) => ExprKind::Pointer(self.compile_pointer(v, &expr.ty)),
            ir::ExprKind::Literal(v) => ExprKind::Literal(self.compile_literal(v)),
            ir::ExprKind::Call(v) => ExprKind::Call(Box::new(self.compile_call(*v))),
            ir::ExprKind::Function(v) => ExprKind::Function(Box::new(self.compile_function(*v))),
            ir::ExprKind::Tuple(v) => ExprKind::Tuple(Box::new(self.compile_tuple(v))),
            ir::ExprKind::Array(v) => ExprKind::Array(Box::new(self.compile_array(expr.ty, v))),
            ir::ExprKind::EnumVariant(v) => {
                ExprKind::EnumVariant(Box::new(self.compile_enum_variant(expr.ty, *v)))
            }
            ir::ExprKind::EnumEq(v) => ExprKind::EnumEq(Box::new(self.compile_enum_eq(*v))),
            ir::ExprKind::EnumUnwrap(v) => return self.compile_enum_unwrap(*v),
            ir::ExprKind::TupleLookup(v) => return self.compile_tuple_lookup(*v),
            ir::ExprKind::Binding(v) => ExprKind::Binding(Box::new(self.compile_binding(*v))),
            ir::ExprKind::Switch(v) => ExprKind::Switch(self.compile_switch(v)),
        };

        Expr { kind }
    }

    fn compile_pointer(&mut self, ptr: ir::Pointer, ty: &ir::Ty) -> Sid {
        match ptr {
            ir::Pointer::External(e_ptr) => {
                let ty = self.get_ty_mat(ty);
                let e_symbol = compile_external_symbol(e_ptr.id, ty);
                let (index, _) = self.externals.insert_full(e_symbol);

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

    fn compile_enum_variant(&mut self, ty: Ty, v: ir::EnumVariant) -> EnumVariant {
        let ir::TyKind::Enum(ty_variants) = &ty.kind else {
            panic!()
        };
        let ty_variant = ty_variants.get(v.tag as usize).unwrap();
        let head_format = lutra_bin::layout::enum_head_format(ty_variants);
        let variant_format = lutra_bin::layout::enum_variant_format(&head_format, &ty_variant.ty);

        EnumVariant {
            tag: v.tag.to_le_bytes()[0..head_format.tag_bytes as usize].to_vec(),
            has_ptr: head_format.has_ptr,
            padding_bytes: variant_format.padding_bytes,
            inner: self.compile_expr(v.inner),
        }
    }

    fn compile_enum_eq(&mut self, v: ir::EnumEq) -> EnumEq {
        let ty_variants = self.get_ty_mat(&v.expr.ty).kind.as_enum().unwrap();
        let head_format = lutra_bin::layout::enum_head_format(ty_variants);

        let tag = v.tag.to_le_bytes()[0..head_format.tag_bytes as usize].to_vec();
        EnumEq {
            tag,
            expr: self.compile_expr(v.expr),
        }
    }

    fn compile_enum_unwrap(&mut self, v: ir::EnumUnwrap) -> Expr {
        let ty_variants = self.get_ty_mat(&v.expr.ty).kind.as_enum().unwrap();
        let head_format = lutra_bin::layout::enum_head_format(ty_variants);

        let mut expr = self.compile_expr(v.expr);

        // offset tag
        expr = Expr {
            kind: ExprKind::Offset(Box::new(Offset {
                base: expr,
                offset: head_format.tag_bytes,
            })),
        };

        // dereference pointer (if there is a pointer)
        if head_format.has_ptr {
            expr = Expr {
                kind: ExprKind::Deref(Box::new(Deref { ptr: expr })),
            };
        }

        expr
    }

    fn compile_tuple_lookup(&mut self, value: ir::TupleLookup) -> Expr {
        let base_ty = self.get_ty_mat(&value.base.ty);
        let offset = lutra_bin::layout::tuple_field_offset(base_ty, value.position);

        let kind = ExprKind::Offset(Box::new(Offset {
            base: self.compile_expr(value.base),
            offset,
        }));
        Expr { kind }
    }

    fn compile_binding(&mut self, value: ir::Binding) -> Binding {
        Binding {
            symbol: Sid(value.id).with_tag(SidKind::Var),
            expr: self.compile_expr(value.expr),
            main: self.compile_expr(value.main),
        }
    }

    fn compile_switch(&mut self, branches: Vec<ir::SwitchBranch>) -> Vec<SwitchBranch> {
        branches
            .into_iter()
            .map(|b| SwitchBranch {
                condition: self.compile_expr(b.condition),
                value: self.compile_expr(b.value),
            })
            .collect()
    }

    fn compile_ty_layout(&mut self, value: ir::TyLayout) -> TyLayout {
        TyLayout {
            head_size: value.head_size,
            body_ptrs: value.body_ptrs,
        }
    }
}

fn compile_external_symbol(id: String, ty: &ir::Ty) -> ExternalSymbol {
    let layout_args: Vec<u32> = match id.as_str() {
        "std::mul" | "std::div" | "std::mod" | "std::add" | "std::sub" | "std::neg" => {
            let param_ty = as_ty_of_param(ty);
            let primitive = param_ty.kind.as_primitive().unwrap();

            let mut buf = lutra_bin::bytes::BytesMut::with_capacity(1);
            primitive.encode(&mut buf);
            buf.put_bytes(0, 3); // padding
            vec![buf.get_u32()]
        }

        "std::count" => vec![],

        "std::index" | "std::min" | "std::max" | "std::sum" | "std::average" | "std::contains" => {
            let item_layout = as_layout_of_param_array(ty);
            vec![
                item_layout.head_size.div_ceil(8), // item_head_size
            ]
        }

        "std::filter" | "std::slice" | "std::sort" | "std::lag" | "std::lead" => {
            let item_layout = as_layout_of_param_array(ty);

            let mut r = Vec::with_capacity(1 + 1 + item_layout.body_ptrs.len());
            r.push(item_layout.head_size.div_ceil(8)); // item_head_size
            r.extend(as_len_and_items(&item_layout.body_ptrs)); // item_body_ptrs
            r
        }

        "std::map" => {
            let input_layout = as_layout_of_param_array(ty);
            let output_layout = as_layout_of_return_array(ty);

            let mut r = Vec::with_capacity(2 + 1 + output_layout.body_ptrs.len());
            r.push(input_layout.head_size.div_ceil(8)); // input_item_head
            r.push(output_layout.head_size.div_ceil(8)); // output_item_head
            r.extend(as_len_and_items(&output_layout.body_ptrs)); // output_item_body_ptrs
            r
        }

        "std::to_columnar" => {
            let ty_func = ty.kind.as_function().unwrap();

            let input_item = ty_func.params[0].kind.as_array().unwrap();
            let input_layout = input_item.layout.as_ref().unwrap();

            let mut r = Vec::new();
            r.push(input_layout.head_size.div_ceil(8)); // item_head_size

            let input_field_offsets = lutra_bin::layout::tuple_field_offsets(input_item);
            r.extend(as_len_and_items(&input_field_offsets)); // field_offsets

            // fields_head_bytes
            let fields = input_item.kind.as_tuple().unwrap();
            r.push(fields.len() as u32);
            for field in fields {
                let field_layout = field.ty.layout.as_ref().unwrap();
                r.push(field_layout.head_size.div_ceil(8));
            }

            // fields_body_ptrs
            for field in fields {
                let field_layout = field.ty.layout.as_ref().unwrap();
                r.extend(as_len_and_items(&field_layout.body_ptrs));
            }

            r
        }
        "std::from_columnar" => {
            let ty_func = ty.kind.as_function().unwrap();

            let output_item = ty_func.body.kind.as_array().unwrap();
            let output_layout = output_item.layout.as_ref().unwrap();

            let mut r = Vec::new();
            r.push(output_layout.head_size.div_ceil(8)); // output_head_bytes

            r.extend(as_len_and_items(&output_layout.body_ptrs)); // output_body_ptrs

            // fields_item_head_bytes
            let fields = output_item.kind.as_tuple().unwrap();
            r.push(fields.len() as u32);
            for field in fields {
                let field_layout = field.ty.layout.as_ref().unwrap();
                r.push(field_layout.head_size.div_ceil(8));
            }

            // fields_body_ptrs
            for field in fields {
                let field_layout = field.ty.layout.as_ref().unwrap();
                r.extend(as_len_and_items(&field_layout.body_ptrs));
            }

            r
        }

        _ => vec![],
    };
    ExternalSymbol { id, layout_args }
}

fn as_len_and_items(items: &[u32]) -> impl Iterator<Item = u32> + '_ {
    Some(items.len() as u32)
        .into_iter()
        .chain(items.iter().cloned())
}

fn as_layout_of_param_array(ty: &Ty) -> &ir::TyLayout {
    let ty_func = ty.kind.as_function().unwrap();
    let ty_array = ty_func.params[0].kind.as_array().unwrap();
    let ty_layout = ty_array.layout.as_ref().unwrap();
    ty_layout
}

fn as_layout_of_return_array(ty: &Ty) -> &ir::TyLayout {
    let ty_func = ty.kind.as_function().unwrap();
    let ty_array = ty_func.body.kind.as_array().unwrap();
    let ty_layout = ty_array.layout.as_ref().unwrap();
    ty_layout
}

fn as_ty_of_param(ty: &Ty) -> &ir::Ty {
    let ty_func = ty.kind.as_function().unwrap();
    &ty_func.params[0]
}
