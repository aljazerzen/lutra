use std::collections::HashMap;

use indexmap::IndexSet;
use lutra_bin::Encode;
use lutra_bin::br::statics::*;
use lutra_bin::br::*;
use lutra_bin::ir;

pub fn compile_program(value: ir::Program) -> Program {
    let mut b = ByteCoder {
        externals: Default::default(),
        include_defs: false,

        defs: &value.defs,
        def_map: value.defs.iter().map(|def| (&def.name, &def.ty)).collect(),
        next_wrapper_id: 0xFF00,
    };

    let program = Program {
        main: b.compile_expr(value.main),
        externals: b.externals.into_iter().collect(),
        defs: if b.include_defs { value.defs } else { vec![] },
    };
    tracing::debug!("br:\n{program:#?}");
    program
}

struct ByteCoder<'t> {
    externals: IndexSet<ExternalSymbol>,
    defs: &'t [ir::TyDef],
    def_map: HashMap<&'t ir::Path, &'t ir::Ty>,

    // Some externals need defs (read_parquet), but most of the time, skip them.
    include_defs: bool,

    /// Counter for generating unique wrapper function IDs.
    next_wrapper_id: u32,
}

impl<'t> ByteCoder<'t> {
    fn get_ty_mat<'a: 't>(&self, ty: &'a ir::Ty) -> &'t ir::Ty {
        let mut ty = ty;
        while let TyKind::Ident(path) = &ty.kind {
            ty = self.def_map.get(path).unwrap();
        }
        ty
    }
    fn get_ty_mat_or_std<'a: 't>(&self, ty: &'a ir::Ty) -> &'t ir::Ty {
        let mut ty = ty;
        while let TyKind::Ident(path) = &ty.kind {
            if ir::TyStd::try_new(path).is_some() {
                return ty;
            }
            ty = self.def_map.get(path).unwrap();
        }
        ty
    }

    fn compile_expr(&mut self, expr: ir::Expr) -> Expr {
        let kind = match expr.kind {
            ir::ExprKind::Pointer(v) => self.compile_pointer(v, &expr.ty),
            ir::ExprKind::Literal(v) => ExprKind::Literal(self.compile_literal(v)),
            ir::ExprKind::Call(v) => ExprKind::Call(Box::new(self.compile_call(*v))),
            ir::ExprKind::Function(v) => ExprKind::Function(Box::new(self.compile_function(*v))),
            ir::ExprKind::Tuple(v) => ExprKind::Tuple(Box::new(self.compile_tuple(v))),
            ir::ExprKind::Array(v) => ExprKind::Array(Box::new(self.compile_array(expr.ty, v))),
            ir::ExprKind::EnumVariant(v) => {
                ExprKind::EnumVariant(Box::new(self.compile_enum_variant(expr.ty, *v)))
            }
            ir::ExprKind::EnumTag(v) => self.compile_expr(v.subject).kind,
            ir::ExprKind::EnumUnwrap(v) => return self.compile_enum_unwrap(*v),
            ir::ExprKind::TupleLookup(v) => return self.compile_tuple_lookup(*v),
            ir::ExprKind::Binding(v) => ExprKind::Binding(Box::new(self.compile_binding(*v))),
            ir::ExprKind::Switch(v) => ExprKind::Switch(self.compile_switch(v)),
        };

        Expr { kind }
    }

    fn compile_pointer(&mut self, ptr: ir::Pointer, ty: &ir::Ty) -> ExprKind {
        match ptr {
            ir::Pointer::External(e_ptr) => {
                let ty = self.get_ty_mat(ty);
                self.compile_external_symbol(e_ptr.id, ty)
            }
            #[rustfmt::skip]
            ir::Pointer::Binding(binding_id) => {
                ExprKind::Pointer(Sid(binding_id).with_tag(SidKind::Var))
            },
            ir::Pointer::Parameter(param_ptr) => {
                let sid = param_ptr.function_id << 8 | param_ptr.param_position as u32;

                ExprKind::Pointer(Sid(sid).with_tag(SidKind::FunctionScope))
            }
        }
    }

    fn compile_literal(&mut self, value: ir::Literal) -> Vec<u8> {
        match value {
            ir::Literal::Prim8(v) => v.encode(),
            ir::Literal::Prim16(v) => v.encode(),
            ir::Literal::Prim32(v) => v.encode(),
            ir::Literal::Prim64(v) => v.encode(),
            ir::Literal::Text(v) => v.encode(),
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
            .map(|ty| ty_layout(ty.layout.as_ref().unwrap()))
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
        let ty_item = self.get_ty_mat(&ty).kind.as_array().unwrap();
        Array {
            items: items.into_iter().map(|x| self.compile_expr(x)).collect(),
            item_layout: ty_layout(ty_item.layout.as_ref().unwrap()),
        }
    }

    fn compile_enum_variant(&mut self, ty: Ty, v: ir::EnumVariant) -> EnumVariant {
        let ty_mat = self.get_ty_mat(&ty);
        let ir::TyKind::Enum(ty_variants) = &ty_mat.kind else {
            panic!()
        };
        let ty_variant = ty_variants.get(v.tag as usize).unwrap();
        let head_format = lutra_bin::layout::enum_head_format(ty_variants, &ty.variants_recursive);
        let variant_format = lutra_bin::layout::enum_variant_format(&head_format, &ty_variant.ty);

        EnumVariant {
            tag: v.tag.to_le_bytes()[0..head_format.tag_bytes as usize].to_vec(),
            inner_bytes: head_format.inner_bytes as u8,
            has_ptr: head_format.has_ptr,
            padding_bytes: variant_format.padding_bytes,
            inner: self.compile_expr(v.inner),
        }
    }

    fn compile_enum_unwrap(&mut self, v: ir::EnumUnwrap) -> Expr {
        let ty_mat = self.get_ty_mat(&v.subject.ty);
        let ir::TyKind::Enum(ty_variants) = &ty_mat.kind else {
            panic!()
        };

        let head_format =
            lutra_bin::layout::enum_head_format(ty_variants, &ty_mat.variants_recursive);

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

    fn compile_external_symbol(&mut self, id: String, ty_mat: &ir::Ty) -> ExprKind {
        let static_args: Vec<u8> = match id.as_str() {
            "std::ops::add"
            | "std::ops::sub"
            | "std::ops::mul"
            | "std::ops::div"
            | "std::ops::mod"
            | "std::ops::neg"
            | "std::ops::cmp"
            | "std::ops::eq"
            | "std::ops::lt"
            | "std::ops::lte"
            | "std::convert::to_int8"
            | "std::convert::to_int16"
            | "std::convert::to_int32"
            | "std::convert::to_int64"
            | "std::convert::to_uint8"
            | "std::convert::to_uint16"
            | "std::convert::to_uint32"
            | "std::convert::to_uint64"
            | "std::convert::to_float32"
            | "std::convert::to_float64"
            | "std::convert::to_text"
            | "std::math::abs"
            | "std::math::pow"
            | "std::array::sequence" => {
                let param_ty = as_ty_of_param(ty_mat);
                let ty_name = self.as_std_ty_suffix(param_ty);
                return self.make_external(format!("{id}_{ty_name}"), vec![]);
            }

            "std::array::fold" => {
                let item_layout = as_layout_of_param_array(ty_mat);
                item_head_arg(item_layout).encode()
            }

            "std::array::min"
            | "std::array::max"
            | "std::array::rank"
            | "std::array::rank_dense"
            | "std::array::rank_percentile"
            | "std::array::cume_dist" => {
                // These take func([T]): ... — inject cmp for item type T
                let param_ty = as_ty_of_param(ty_mat);
                let item_ty = self.get_ty_mat(param_ty).kind.as_array().unwrap();
                let item_layout = item_ty.layout.as_ref().unwrap();
                let static_args = item_head_arg(item_layout).encode();

                let n_params = ty_mat.kind.as_function().unwrap().params.len();
                let cmp_id = format!("std::ops::cmp_{}", self.as_std_ty_suffix(item_ty));
                let cmp = self.make_external(cmp_id, vec![]);
                return self.wrap_external_with_extra_args(id, static_args, n_params, vec![cmp]);
            }

            "std::array::sort" => {
                let item_layout = as_layout_of_param_array(ty_mat);
                let static_args = ItemLayoutArgs {
                    item: ty_layout(item_layout),
                }
                .encode();

                let ty_func = ty_mat.kind.as_function().unwrap();
                let key_extractor_ty = self.get_ty_mat(&ty_func.params[1]);
                let key_ty = &key_extractor_ty.kind.as_function().unwrap().body;

                let n_params = ty_func.params.len();
                let cmp_id = format!("std::ops::cmp_{}", self.as_std_ty_suffix(key_ty));
                let cmp = self.make_external(cmp_id, vec![]);
                return self.wrap_external_with_extra_args(id, static_args, n_params, vec![cmp]);
            }

            "std::array::sum" | "std::array::mean" | "std::array::rolling_mean" => {
                let param_ty = as_ty_of_param(ty_mat);
                let item_ty = self.get_ty_mat(param_ty).kind.as_array().unwrap();
                let item_layout = item_ty.layout.as_ref().unwrap();
                let static_args = item_head_arg(item_layout).encode();

                let ty_name = self.as_std_ty_suffix(item_ty);
                return self.make_external(format!("{id}_{ty_name}"), static_args);
            }

            "std::array::index" => {
                let item_layout = as_layout_of_param_array(ty_mat);

                let ty_func = ty_mat.kind.as_function().unwrap();
                let ty_out_variants = self.get_ty_mat(&ty_func.body).kind.as_enum().unwrap();
                let ty_out_format = lutra_bin::layout::enum_format(
                    ty_out_variants,
                    &ty_func.body.variants_recursive,
                );

                IndexArgs {
                    item_head: head_bytes(item_layout),
                    output_enum_format: ty_out_format.encode(),
                }
                .encode()
            }

            "std::array::filter"
            | "std::array::slice"
            | "std::array::append"
            | "std::array::loop_until_empty" => {
                let item_layout = as_layout_of_param_array(ty_mat);
                ItemLayoutArgs {
                    item: ty_layout(item_layout),
                }
                .encode()
            }

            "std::array::lag" | "std::array::lead" => {
                let item_layout = as_layout_of_param_array(ty_mat);

                let ty_func = ty_mat.kind.as_function().unwrap();
                let ty_item = ty_func.body.kind.as_array().unwrap();
                let default = self.construct_default_for_ty(ty_item);

                ShiftArgs {
                    item: ty_layout(item_layout),
                    default,
                }
                .encode()
            }

            "std::array::map" | "std::array::flat_map" | "std::array::scan" => {
                let input_layout = as_layout_of_param_array(ty_mat);
                let output_layout = as_layout_of_return_array(ty_mat);

                MapArgs {
                    input_item_head: head_bytes(input_layout),
                    output_item: ty_layout(output_layout),
                }
                .encode()
            }

            "std::array::to_columnar" => {
                let ty_func = ty_mat.kind.as_function().unwrap();

                let input_item = ty_func.params[0].kind.as_array().unwrap();
                let input_layout = input_item.layout.as_ref().unwrap();
                let fields = input_item.kind.as_tuple().unwrap();

                ToColumnarArgs {
                    item_head: head_bytes(input_layout),
                    field_offsets: lutra_bin::layout::tuple_field_offsets(input_item),
                    fields: fields
                        .iter()
                        .map(|f| ty_layout(f.ty.layout.as_ref().unwrap()))
                        .collect(),
                }
                .encode()
            }
            "std::array::from_columnar" => {
                let ty_func = ty_mat.kind.as_function().unwrap();

                let output_item = ty_func.body.kind.as_array().unwrap();
                let output_layout = output_item.layout.as_ref().unwrap();
                let fields = output_item.kind.as_tuple().unwrap();

                FromColumnarArgs {
                    output: ty_layout(output_layout),
                    fields: fields
                        .iter()
                        .map(|f| ty_layout(f.ty.layout.as_ref().unwrap()))
                        .collect(),
                }
                .encode()
            }

            "std::array::zip" => {
                let ty_func = ty_mat.kind.as_function().unwrap();

                let a_item = self.get_ty_mat(&ty_func.params[0]).kind.as_array().unwrap();
                let b_item = self.get_ty_mat(&ty_func.params[1]).kind.as_array().unwrap();

                ZipArgs {
                    a: ty_layout(a_item.layout.as_ref().unwrap()),
                    b: ty_layout(b_item.layout.as_ref().unwrap()),
                }
                .encode()
            }

            "std::array::group" => {
                let ty_func = ty_mat.kind.as_function().unwrap();

                let input_item = self.get_ty_mat(&ty_func.params[0]).kind.as_array().unwrap();
                let input_layout = input_item.layout.as_ref().unwrap();

                let output_item = self.get_ty_mat(&ty_func.body).kind.as_array().unwrap();
                let output_layout = output_item.layout.as_ref().unwrap();
                let fields = output_item.kind.as_tuple().unwrap();

                let key = &self.get_ty_mat(output_item).kind.as_tuple().unwrap()[0].ty;
                let key_layout = key.layout.as_ref().unwrap();

                GroupArgs {
                    input: ty_layout(input_layout),
                    output: ty_layout(output_layout),
                    output_fields: fields
                        .iter()
                        .map(|f| ty_layout(f.ty.layout.as_ref().unwrap()))
                        .collect(),
                    key_head: head_bytes(key_layout),
                }
                .encode()
            }

            "std::fs::read_parquet" => {
                let ty_func = ty_mat.kind.as_function().unwrap();
                let ty_data = ty_func.body.clone();

                self.include_defs = true;
                InjectTypeArgs { ty: ty_data }.encode()
            }
            "std::fs::write_parquet" => {
                let ty_data = as_ty_of_param(ty_mat).clone();

                self.include_defs = true;
                InjectTypeArgs { ty: ty_data }.encode()
            }

            _ => vec![],
        };

        self.make_external(id, static_args)
    }

    /// Determines the snake_case suffix for a framed std type
    fn as_std_ty_suffix(&self, ty: &ir::Ty) -> String {
        match &self.get_ty_mat_or_std(ty).kind {
            ir::TyKind::Ident(path) => path.0.last().unwrap().to_ascii_lowercase(),
            ir::TyKind::Primitive(ir::TyPrimitive::Prim8) => "uint8".into(),
            ir::TyKind::Primitive(ir::TyPrimitive::Prim16) => "uint16".into(),
            ir::TyKind::Primitive(ir::TyPrimitive::Prim32) => "uint32".into(),
            ir::TyKind::Primitive(ir::TyPrimitive::Prim64) => "uint64".into(),
            _ => {
                panic!("std specialization not supported for {}", ir::print_ty(ty));
            }
        }
    }

    /// Registers an external symbol with encoded static args (a per-intrinsic
    /// schema, or empty) and returns a `Pointer` ExprKind for it.
    fn make_external(&mut self, id: String, static_args: Vec<u8>) -> ExprKind {
        let (index, _) = self
            .externals
            .insert_full(ExternalSymbol { id, static_args });
        ExprKind::Pointer(Sid(index as u32).with_tag(SidKind::External))
    }

    /// Wraps an external function in a `Function` that forwards the original
    /// params and appends extra args (e.g. an injected comparator).
    ///
    /// Produces:
    /// ```text
    /// Function(params: [p0, ..., pN-1]) {
    ///     Call(
    ///         func: ExternalPointer(id),
    ///         args: [p0, ..., pN-1, extra0, extra1, ...]
    ///     )
    /// }
    /// ```
    fn wrap_external_with_extra_args(
        &mut self,
        id: String,
        static_args: Vec<u8>,
        n_params: usize,
        extra_args: Vec<ExprKind>,
    ) -> ExprKind {
        // Register the real external symbol
        let (ext_index, _) = self
            .externals
            .insert_full(ExternalSymbol { id, static_args });
        let ext_sid = Sid(ext_index as u32).with_tag(SidKind::External);

        // Generate a unique wrapper function ID
        let wrapper_id = self.next_wrapper_id;
        self.next_wrapper_id += 1;
        let wrapper_ns = Sid(wrapper_id << 8).with_tag(SidKind::FunctionScope);

        // Build args: forward original params + append extras
        let mut args: Vec<Expr> = (0..n_params)
            .map(|i| {
                let sid = Sid(wrapper_id << 8 | i as u32).with_tag(SidKind::FunctionScope);
                Expr {
                    kind: ExprKind::Pointer(sid),
                }
            })
            .collect();
        args.extend(extra_args.into_iter().map(|kind| Expr { kind }));

        ExprKind::Function(Box::new(Function {
            symbol_ns: wrapper_ns,
            body: Expr {
                kind: ExprKind::Call(Box::new(Call {
                    function: Expr {
                        kind: ExprKind::Pointer(ext_sid),
                    },
                    args,
                })),
            },
        }))
    }

    fn construct_default_for_ty(&self, ty: &ir::Ty) -> Vec<u8> {
        self.construct_default_for_ty_re(ty)
            .encode(ty, self.defs)
            .unwrap()
    }

    fn construct_default_for_ty_re(&self, ty: &ir::Ty) -> lutra_bin::Value {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(prim) => match prim {
                ir::TyPrimitive::Prim8 => lutra_bin::Value::Prim8(0),
                ir::TyPrimitive::Prim16 => lutra_bin::Value::Prim16(0),
                ir::TyPrimitive::Prim32 => lutra_bin::Value::Prim32(0),
                ir::TyPrimitive::Prim64 => lutra_bin::Value::Prim64(0),
            },
            ir::TyKind::Array(_) => lutra_bin::Value::Array(vec![]),
            ir::TyKind::Tuple(ty_fields) => lutra_bin::Value::Tuple(
                ty_fields
                    .iter()
                    .map(|f| self.construct_default_for_ty_re(&f.ty))
                    .collect(),
            ),
            ir::TyKind::Enum(ty_enum_variants) => {
                let variant = ty_enum_variants.iter().next().unwrap();
                lutra_bin::Value::Enum(0, Box::new(self.construct_default_for_ty_re(&variant.ty)))
            }

            ir::TyKind::Function(_) => panic!(),
            ir::TyKind::Ident(_) => unreachable!(),
        }
    }
}

fn item_head_arg(layout: &ir::TyLayout) -> statics::ItemHeadArgs {
    statics::ItemHeadArgs {
        item_head: head_bytes(layout),
    }
}

/// Byte size of a type's head (the codec stores head sizes in bytes, while
/// `ir::TyLayout::head_size` is in bits).
fn head_bytes(layout: &ir::TyLayout) -> u32 {
    layout.head_size.div_ceil(8)
}

fn ty_layout(l: &ir::TyLayout) -> TyLayout {
    TyLayout {
        head_bytes: head_bytes(l),
        body_ptrs: l.body_ptrs.clone(),
    }
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
