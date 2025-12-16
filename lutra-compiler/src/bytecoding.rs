use std::collections::HashMap;

use indexmap::IndexSet;
use lutra_bin::Encode;
use lutra_bin::br::*;
use lutra_bin::bytes::BufMut;
use lutra_bin::ir;

pub fn compile_program(value: ir::Program) -> Program {
    let mut b = ByteCoder {
        externals: Default::default(),
        defs: value
            .defs
            .into_iter()
            .map(|def| (def.name, def.ty))
            .collect(),
    };

    Program {
        main: b.compile_expr(value.main),
        externals: b.externals.into_iter().collect(),
    }
}

struct ByteCoder {
    externals: IndexSet<ExternalSymbol>,
    defs: HashMap<ir::Path, ir::Ty>,
}

impl ByteCoder {
    fn get_ty_mat<'a>(&'a self, ty: &'a ir::Ty) -> &'a ir::Ty {
        match &ty.kind {
            TyKind::Ident(path) => self.defs.get(path).unwrap(),
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
                let e_symbol = self.compile_external_symbol(e_ptr.id, ty);
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

    fn compile_literal(&mut self, value: ir::Literal) -> Vec<u8> {
        match value {
            ir::Literal::bool(v) => v.encode(),
            ir::Literal::int8(v) => v.encode(),
            ir::Literal::int16(v) => v.encode(),
            ir::Literal::int32(v) => v.encode(),
            ir::Literal::int64(v) => v.encode(),
            ir::Literal::uint8(v) => v.encode(),
            ir::Literal::uint16(v) => v.encode(),
            ir::Literal::uint32(v) => v.encode(),
            ir::Literal::uint64(v) => v.encode(),
            ir::Literal::float32(v) => v.encode(),
            ir::Literal::float64(v) => v.encode(),
            ir::Literal::text(v) => v.encode(),
        }
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

    fn compile_tuple(&mut self, fields: Vec<ir::TupleField>) -> Tuple {
        let field_layouts = fields
            .iter()
            .flat_map(|f| {
                if f.unpack {
                    let ir::TyKind::Tuple(fields) = &self.get_ty_mat(&f.expr.ty).kind else {
                        panic!();
                    };
                    fields.iter().map(|f| &f.ty).collect::<Vec<_>>()
                } else {
                    vec![&f.expr.ty]
                }
            })
            .map(|ty| self.compile_ty_layout(ty.layout.clone().unwrap()))
            .collect();

        let fields = fields
            .into_iter()
            .map(|f| {
                let unpack = if f.unpack {
                    let ir::TyKind::Tuple(fields) = &self.get_ty_mat(&f.expr.ty).kind else {
                        panic!();
                    };
                    fields.len() as u8
                } else {
                    0
                };
                let expr = self.compile_expr(f.expr);

                TupleField { expr, unpack }
            })
            .collect();
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
        let ir::TyKind::Enum(ty_variants) = &self.get_ty_mat(&ty).kind else {
            panic!()
        };
        let ty_variant = ty_variants.get(v.tag as usize).unwrap();
        let head_format = lutra_bin::layout::enum_head_format(ty_variants);
        let variant_format = lutra_bin::layout::enum_variant_format(&head_format, &ty_variant.ty);

        EnumVariant {
            tag: v.tag.to_le_bytes()[0..head_format.tag_bytes as usize].to_vec(),
            inner_bytes: head_format.inner_bytes as u8,
            has_ptr: head_format.has_ptr,
            padding_bytes: variant_format.padding_bytes,
            inner: self.compile_expr(v.inner),
        }
    }

    fn compile_enum_eq(&mut self, v: ir::EnumEq) -> EnumEq {
        let ty_variants = self.get_ty_mat(&v.subject.ty).kind.as_enum().unwrap();
        let head_format = lutra_bin::layout::enum_head_format(ty_variants);

        let tag = v.tag.to_le_bytes()[0..head_format.tag_bytes as usize].to_vec();
        EnumEq {
            tag,
            expr: self.compile_expr(v.subject),
        }
    }

    fn compile_enum_unwrap(&mut self, v: ir::EnumUnwrap) -> Expr {
        let ty_variants = self.get_ty_mat(&v.subject.ty).kind.as_enum().unwrap();
        let head_format = lutra_bin::layout::enum_head_format(ty_variants);

        let mut expr = self.compile_expr(v.subject);

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

    fn compile_ty_layout(&self, value: ir::TyLayout) -> TyLayout {
        TyLayout {
            head_size: value.head_size,
            body_ptrs: value.body_ptrs,
        }
    }

    fn compile_external_symbol(&self, id: String, ty_mat: &ir::Ty) -> ExternalSymbol {
        let layout_args: Vec<u32> = match id.as_str() {
            "std::to_int8" | "std::to_int16" | "std::to_int32" | "std::to_int64"
            | "std::to_uint8" | "std::to_uint16" | "std::to_uint32" | "std::to_uint64"
            | "std::to_float32" | "std::to_float64" | "std::mul" | "std::div" | "std::mod"
            | "std::add" | "std::sub" | "std::neg" | "std::cmp" | "std::eq" | "std::lt"
            | "std::lte" | "std::sequence" | "std::math::abs" | "std::math::pow" => {
                let param_ty = as_ty_of_param(ty_mat);
                let primitive = param_ty.kind.as_primitive().unwrap();

                vec![encode_prim(primitive)]
            }

            "std::count" => vec![],

            "std::min" | "std::max" | "std::fold" => {
                let item_layout = as_layout_of_param_array(ty_mat);
                vec![
                    item_layout.head_size.div_ceil(8), // item_head_size
                ]
            }

            "std::sum"
            | "std::mean"
            | "std::rolling_mean"
            | "std::rank"
            | "std::rank_dense"
            | "std::rank_percentile"
            | "std::cume_dist" => {
                let param_ty = as_ty_of_param(ty_mat);
                let item_ty = self.get_ty_mat(param_ty).kind.as_array().unwrap();

                let item_layout = item_ty.layout.as_ref().unwrap();
                let item_ty = item_ty.kind.as_primitive().unwrap();

                vec![
                    item_layout.head_size.div_ceil(8), // item_head_size
                    encode_prim(item_ty),
                ]
            }

            "std::index" => {
                let item_layout = as_layout_of_param_array(ty_mat);

                let ty_func = ty_mat.kind.as_function().unwrap();
                let ty_out_variants = ty_func.body.kind.as_enum().unwrap();
                let ty_out_format = lutra_bin::layout::enum_format(ty_out_variants);
                let ty_out_format = ty_out_format.encode();

                let mut r = vec![
                    item_layout.head_size.div_ceil(8), // item_head_size
                ];

                pack_bytes_to_u32(ty_out_format, &mut r);
                r
            }

            "std::filter" | "std::slice" | "std::append" | "std::apply_until_empty" => {
                let item_layout = as_layout_of_param_array(ty_mat);

                let mut r = Vec::with_capacity(1 + 1 + item_layout.body_ptrs.len());
                r.push(item_layout.head_size.div_ceil(8)); // item_head_size
                r.extend(as_len_and_items(&item_layout.body_ptrs)); // item_body_ptrs
                r
            }
            "std::sort" => {
                let item_layout = as_layout_of_param_array(ty_mat);

                let mut r = Vec::with_capacity(1 + 1 + item_layout.body_ptrs.len());
                r.push(item_layout.head_size.div_ceil(8)); // item_head_size
                r.extend(as_len_and_items(&item_layout.body_ptrs)); // item_body_ptrs

                // ty of key
                let ty_func = ty_mat.kind.as_function().unwrap();
                let ty_key_extractor = self.get_ty_mat(&ty_func.params[1]);
                let ty_key_extractor = ty_key_extractor.kind.as_function().unwrap();
                let ty_key = self.get_ty_mat(&ty_key_extractor.body);
                r.push(encode_prim(ty_key.kind.as_primitive().unwrap()));

                r
            }

            "std::lag" | "std::lead" => {
                let item_layout = as_layout_of_param_array(ty_mat);

                let mut r = Vec::with_capacity(1 + 1 + item_layout.body_ptrs.len());
                r.push(item_layout.head_size.div_ceil(8)); // item_head_size
                r.extend(as_len_and_items(&item_layout.body_ptrs)); // item_body_ptrs

                // also encode default value
                let ty_func = ty_mat.kind.as_function().unwrap();
                let ty_item = ty_func.body.kind.as_array().unwrap();
                let default_val = self.construct_default_for_ty(ty_item);
                let default_val = default_val.encode(ty_item, &[]).unwrap(); // TODO: ty_defs
                pack_bytes_to_u32(default_val, &mut r);

                r
            }

            "std::map" | "std::flat_map" | "std::scan" => {
                let input_layout = as_layout_of_param_array(ty_mat);
                let output_layout = as_layout_of_return_array(ty_mat);

                let mut r = Vec::with_capacity(2 + 1 + output_layout.body_ptrs.len());
                r.push(input_layout.head_size.div_ceil(8)); // input_item_head
                r.push(output_layout.head_size.div_ceil(8)); // output_item_head
                r.extend(as_len_and_items(&output_layout.body_ptrs)); // output_item_body_ptrs
                r
            }

            "std::to_columnar" => {
                let ty_func = ty_mat.kind.as_function().unwrap();

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
                let ty_func = ty_mat.kind.as_function().unwrap();

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

            "std::zip" => {
                let ty_func = ty_mat.kind.as_function().unwrap();

                let a_item = self.get_ty_mat(&ty_func.params[0]).kind.as_array().unwrap();
                let a_layout = a_item.layout.as_ref().unwrap();

                let b_item = self.get_ty_mat(&ty_func.params[1]).kind.as_array().unwrap();
                let b_layout = b_item.layout.as_ref().unwrap();

                let mut r = Vec::new();
                r.push(a_layout.head_size.div_ceil(8));
                r.extend(as_len_and_items(&a_layout.body_ptrs));
                r.push(b_layout.head_size.div_ceil(8));
                r.extend(as_len_and_items(&b_layout.body_ptrs));
                r
            }

            "std::group" => {
                let ty_func = ty_mat.kind.as_function().unwrap();

                let input_item = &ty_func.params[0].kind.as_array().unwrap();
                let input_layout = input_item.layout.as_ref().unwrap();

                let output_item = ty_func.body.kind.as_array().unwrap();
                let output_layout = output_item.layout.as_ref().unwrap();

                let mut r = Vec::new();
                r.push(input_layout.head_size.div_ceil(8)); // input_head_bytes
                r.extend(as_len_and_items(&input_layout.body_ptrs)); // input_body_ptrs

                r.push(output_layout.head_size.div_ceil(8)); // output_head_bytes
                r.extend(as_len_and_items(&output_layout.body_ptrs)); // output_body_ptrs

                // output_field_head_bytes
                let fields = output_item.kind.as_tuple().unwrap();
                r.push(fields.len() as u32);
                for field in fields {
                    let field_layout = field.ty.layout.as_ref().unwrap();
                    r.push(field_layout.head_size.div_ceil(8));
                }

                // output_fields_body_ptrs
                for field in fields {
                    let field_layout = field.ty.layout.as_ref().unwrap();
                    r.extend(as_len_and_items(&field_layout.body_ptrs));
                }

                r
            }

            "std::fs::read_parquet" => {
                let ty_func = ty_mat.kind.as_function().unwrap();
                let array_item = self.get_ty_mat(&ty_func.body).kind.as_array().unwrap();

                let array_item_buf = array_item.encode();
                let mut r = Vec::new();
                pack_bytes_to_u32(array_item_buf, &mut r);
                r
            }
            "std::fs::write_parquet" => {
                let array = self.get_ty_mat(as_ty_of_param(ty_mat));
                let array_item = array.kind.as_array().unwrap();

                let array_item_buf = array_item.encode();
                let mut r = Vec::new();
                pack_bytes_to_u32(array_item_buf, &mut r);
                r
            }

            _ => vec![],
        };
        ExternalSymbol { id, layout_args }
    }

    fn construct_default_for_ty(&self, ty: &ir::Ty) -> lutra_bin::Value {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(prim) => match prim {
                ir::TyPrimitive::bool | ir::TyPrimitive::int8 | ir::TyPrimitive::uint8 => {
                    lutra_bin::Value::Prim8(0)
                }

                ir::TyPrimitive::int16 | ir::TyPrimitive::uint16 => lutra_bin::Value::Prim16(0),

                ir::TyPrimitive::int32 | ir::TyPrimitive::uint32 | ir::TyPrimitive::float32 => {
                    lutra_bin::Value::Prim32(0)
                }

                ir::TyPrimitive::int64 | ir::TyPrimitive::uint64 | ir::TyPrimitive::float64 => {
                    lutra_bin::Value::Prim64(0)
                }
                ir::TyPrimitive::text => lutra_bin::Value::Text("".into()),
            },
            ir::TyKind::Array(_) => lutra_bin::Value::Array(vec![]),
            ir::TyKind::Tuple(ty_fields) => lutra_bin::Value::Tuple(
                ty_fields
                    .iter()
                    .map(|f| self.construct_default_for_ty(&f.ty))
                    .collect(),
            ),
            ir::TyKind::Enum(ty_enum_variants) => {
                let variant = ty_enum_variants.iter().next().unwrap();
                lutra_bin::Value::Enum(0, Box::new(self.construct_default_for_ty(&variant.ty)))
            }

            ir::TyKind::Function(_) => panic!(),
            ir::TyKind::Ident(_) => unreachable!(),
        }
    }
}

fn encode_prim(primitive: &ir::TyPrimitive) -> u32 {
    let mut buf = primitive.encode();
    buf.put_bytes(0, 3);
    // padding
    u32::from_be_bytes(buf[0..4].try_into().unwrap())
}

fn as_len_and_items(items: &[u32]) -> impl Iterator<Item = u32> + '_ {
    Some(items.len() as u32)
        .into_iter()
        .chain(items.iter().cloned())
}

fn as_layout_of_param_array(ty: &Ty) -> &ir::TyLayout {
    let ty_func = ty.kind.as_function().unwrap();
    let ty_array = ty_func.params[0].kind.as_array().unwrap();

    ty_array.layout.as_ref().unwrap()
}

fn as_layout_of_return_array(ty: &Ty) -> &ir::TyLayout {
    let ty_func = ty.kind.as_function().unwrap();
    let ty_array = ty_func.body.kind.as_array().unwrap();

    ty_array.layout.as_ref().unwrap()
}

fn as_ty_of_param(ty: &Ty) -> &ir::Ty {
    let ty_func = ty.kind.as_function().unwrap();
    &ty_func.params[0]
}

fn pack_bytes_to_u32(mut input: Vec<u8>, output: &mut Vec<u32>) {
    let input_len = input.len();

    // pad
    if !input.len().is_multiple_of(4) {
        input.put_bytes(0, 4 - input.len() % 4);
    }

    // cast to Vec<u32> as le bytes
    output.reserve(2 + input.len() / 4);
    output.push((input.len() / 4) as u32 + 1);
    output.push(input_len as u32);
    for chunk in input.chunks_exact(4) {
        output.push(u32::from_le_bytes(chunk.try_into().unwrap()));
    }
}
