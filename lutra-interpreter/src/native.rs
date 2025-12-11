use lutra_bin::{Encode, Layout};

use crate::interpreter::{Cell, Interpreter};
use crate::{Data, NativeModule};

macro_rules! reduce_func {
    ($name: ident, $item_decode: path, $reduce: expr, $finalize: path) => {
        pub fn $name(
            _: &mut Interpreter,
            layout_args: &[u32],
            args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            let [array] = assume::exactly_n(args);
            let array = assume::array(array, layout_args[0]);

            let res = array.map(|x| $item_decode(&x)).reduce($reduce);

            let res = $finalize(res);

            Ok(Cell::Data(encode(&res)))
        }
    };
}

macro_rules! number_cast {
    ($func_name: ident, $res_ty: ty) => {
        pub fn $func_name(
            _: &mut Interpreter,
            layout_args: &[u32],
            args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            let input_ty = decode::ty_primitive(layout_args[0]);

            let [x] = assume::exactly_n(args);
            let x = match input_ty {
                ir::TyPrimitive::int8 => assume::primitive::<i8>(&x)? as $res_ty,
                ir::TyPrimitive::int16 => assume::primitive::<i16>(&x)? as $res_ty,
                ir::TyPrimitive::int32 => assume::primitive::<i32>(&x)? as $res_ty,
                ir::TyPrimitive::int64 => assume::primitive::<i64>(&x)? as $res_ty,
                ir::TyPrimitive::uint8 => assume::primitive::<u8>(&x)? as $res_ty,
                ir::TyPrimitive::uint16 => assume::primitive::<u16>(&x)? as $res_ty,
                ir::TyPrimitive::uint32 => assume::primitive::<u32>(&x)? as $res_ty,
                ir::TyPrimitive::uint64 => assume::primitive::<u64>(&x)? as $res_ty,
                ir::TyPrimitive::float32 => assume::primitive::<f32>(&x)? as $res_ty,
                ir::TyPrimitive::float64 => assume::primitive::<f64>(&x)? as $res_ty,
                _ => panic!(),
            };
            Ok(Cell::Data(encode(&x)))
        }
    };
}

macro_rules! primitive_op {
    ($args: ident, $prim: ty, $op: ident) => {
        {
            let left = assume::primitive::<$prim>(&$args[0])?;
            let right = assume::primitive::<$prim>(&$args[1])?;
            left.$op(&right)
        }
    };
    ($args: ident, $prim: ty, $op: tt) => {
        {
            let left = assume::primitive::<$prim>(&$args[0])?;
            let right = assume::primitive::<$prim>(&$args[1])?;
            left $op right
        }
    };
}

macro_rules! bin_func {
    ($name: ident, $args_ty: ident, $op: tt, $res_ty: ty) => {
        pub fn $name(
            _: &mut Interpreter,
            _layout_args: &[u32],
            args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            let res = primitive_op!(args, $args_ty, $op);
            Ok(Cell::Data(encode::<$res_ty>(&res)))
        }
    };
}

macro_rules! bin_num_func {
    ($name: ident, $op: tt) => {
        pub fn $name(
            _: &mut Interpreter,
            layout_args: &[u32],
            args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            let prim = decode::ty_primitive(layout_args[0]);

            Ok(match prim {
                ir::TyPrimitive::int8 => encode_cell(&primitive_op!(args, i8, $op)),
                ir::TyPrimitive::int16 => encode_cell(&primitive_op!(args, i16, $op)),
                ir::TyPrimitive::int32 => encode_cell(&primitive_op!(args, i32, $op)),
                ir::TyPrimitive::int64 => encode_cell(&primitive_op!(args, i64, $op)),
                ir::TyPrimitive::uint8 => encode_cell(&primitive_op!(args, u8, $op)),
                ir::TyPrimitive::uint16 => encode_cell(&primitive_op!(args, u16, $op)),
                ir::TyPrimitive::uint32 => encode_cell(&primitive_op!(args, u32, $op)),
                ir::TyPrimitive::uint64 => encode_cell(&primitive_op!(args, u64, $op)),
                ir::TyPrimitive::float32 => encode_cell(&primitive_op!(args, f32, $op)),
                ir::TyPrimitive::float64 => encode_cell(&primitive_op!(args, f64, $op)),
                _ => panic!(),
            })
        }
    };
}

macro_rules! bin_prim_func {
    ($name: ident, $op: ident, $res_ty: ident) => {
        pub fn $name(
            _: &mut Interpreter,
            layout_args: &[u32],
            args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            let prim = decode::ty_primitive(layout_args[0]);

            Ok(Cell::Data(match prim {
                ir::TyPrimitive::bool => encode::<$res_ty>(&primitive_op!(args, bool, $op)),
                ir::TyPrimitive::int8 => encode::<$res_ty>(&primitive_op!(args, i8, $op)),
                ir::TyPrimitive::int16 => encode::<$res_ty>(&primitive_op!(args, i16, $op)),
                ir::TyPrimitive::int32 => encode::<$res_ty>(&primitive_op!(args, i32, $op)),
                ir::TyPrimitive::int64 => encode::<$res_ty>(&primitive_op!(args, i64, $op)),
                ir::TyPrimitive::uint8 => encode::<$res_ty>(&primitive_op!(args, u8, $op)),
                ir::TyPrimitive::uint16 => encode::<$res_ty>(&primitive_op!(args, u16, $op)),
                ir::TyPrimitive::uint32 => encode::<$res_ty>(&primitive_op!(args, u32, $op)),
                ir::TyPrimitive::uint64 => encode::<$res_ty>(&primitive_op!(args, u64, $op)),
                ir::TyPrimitive::float32 => encode::<$res_ty>(&primitive_op!(args, f32, $op)),
                ir::TyPrimitive::float64 => encode::<$res_ty>(&primitive_op!(args, f64, $op)),
                ir::TyPrimitive::text => {
                    let mut left = assume::as_data(&args[0])?.clone();
                    let left = decode::text_ref(&mut left);
                    let mut right = assume::as_data(&args[1])?.clone();
                    let right = decode::text_ref(&mut right);

                    let res = left.$op(right);
                    encode::<$res_ty>(&res)
                }
            }))
        }
    };
}

macro_rules! neg_arg {
    ($prim: ty, $args: ident) => {{
        let operand = assume::primitive::<$prim>(&$args[0])?;
        let res = -operand;
        Cell::Data(encode::<$prim>(&res))
    }};
}

pub mod std {
    use ::std::borrow::Cow;
    use ::std::cmp::Ordering;
    use ::std::collections::HashMap;

    use crate::native::assume::LayoutArgsReader;
    use crate::native::*;
    use crate::{ArrayWriter, Data, EnumWriter, EvalError, TupleWriter};
    use lutra_bin::{ArrayReader, Decode, TupleReader, ir};

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "to_int8" => &to_int8,
                "to_int16" => &to_int16,
                "to_int32" => &to_int32,
                "to_int64" => &to_int64,
                "to_uint8" => &to_uint8,
                "to_uint16" => &to_uint16,
                "to_uint32" => &to_uint32,
                "to_uint64" => &to_uint64,
                "to_float32" => &to_float32,
                "to_float64" => &to_float64,

                "mul" => &mul,
                "div" => &div,
                "mod" => &r#mod,
                "add" => &add,
                "sub" => &sub,
                "neg" => &neg,

                "cmp" => &cmp,
                "eq" => &eq,
                "lt" => &lt,
                "lte" => &lte,

                "and" => &and,
                "or" => &or,
                "not" => &not,

                "index" => &index,
                "map" => &map,
                "flat_map" => &flat_map,
                "filter" => &filter,
                "slice" => &slice,
                "sort" => &sort,
                "to_columnar" => &to_columnar,
                "from_columnar" => &from_columnar,
                "zip" => &zip,
                "group" => &group,
                "append" => &append,
                "fold" => &fold,
                "scan" => &scan,
                "apply_until_empty" => &apply_until_empty,

                "min" => &min,
                "max" => &max,
                "sum" => &sum,
                "mean" => &mean,
                "all" => &all,
                "any" => &any,
                "concat_array" => &concat_array,
                "count" => &count,

                "lag" => &lag,
                "lead" => &lead,
                "row_number" => &row_number,
                "rolling_mean" => &rolling_mean,

                _ => return None,
            })
        }
    }

    number_cast!(to_int8, i8);
    number_cast!(to_int16, i16);
    number_cast!(to_int32, i32);
    number_cast!(to_int64, i64);
    number_cast!(to_uint8, u8);
    number_cast!(to_uint16, u16);
    number_cast!(to_uint32, u32);
    number_cast!(to_uint64, u64);
    number_cast!(to_float32, f32);
    number_cast!(to_float64, f64);

    bin_num_func!(add, +);
    bin_num_func!(sub, -);
    bin_num_func!(mul, *);
    bin_num_func!(div, /);
    bin_num_func!(r#mod, %);

    pub fn neg(
        _: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let ty_prim = decode::ty_primitive(layout_args[0]);
        Ok(match ty_prim {
            ir::TyPrimitive::int8 => neg_arg!(i8, args),
            ir::TyPrimitive::int16 => neg_arg!(i16, args),
            ir::TyPrimitive::int32 => neg_arg!(i32, args),
            ir::TyPrimitive::int64 => neg_arg!(i64, args),
            ir::TyPrimitive::float32 => neg_arg!(f32, args),
            ir::TyPrimitive::float64 => neg_arg!(f64, args),
            _ => panic!(),
        })
    }

    pub fn cmp(
        _: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let tag: u8 = match cmp_raw(layout_args[0], &args)? {
            Ordering::Less => 0,
            Ordering::Equal => 1,
            Ordering::Greater => 2,
        };
        Ok(encode_cell(&tag))
    }

    fn cmp_raw(ty_arg: u32, args: &[Cell]) -> Result<Ordering, EvalError> {
        let ty_prim = decode::ty_primitive(ty_arg);
        Ok(match ty_prim {
            ir::TyPrimitive::bool => primitive_op!(args, bool, cmp),
            ir::TyPrimitive::int8 => primitive_op!(args, i8, cmp),
            ir::TyPrimitive::int16 => primitive_op!(args, i16, cmp),
            ir::TyPrimitive::int32 => primitive_op!(args, i32, cmp),
            ir::TyPrimitive::int64 => primitive_op!(args, i64, cmp),
            ir::TyPrimitive::uint8 => primitive_op!(args, u8, cmp),
            ir::TyPrimitive::uint16 => primitive_op!(args, u16, cmp),
            ir::TyPrimitive::uint32 => primitive_op!(args, u32, cmp),
            ir::TyPrimitive::uint64 => primitive_op!(args, u64, cmp),
            ir::TyPrimitive::float32 => primitive_op!(args, f32, total_cmp),
            ir::TyPrimitive::float64 => primitive_op!(args, f64, total_cmp),
            ir::TyPrimitive::text => {
                let mut left = assume::as_data(&args[0])?.clone();
                let left = decode::text_ref(&mut left);
                let mut right = assume::as_data(&args[1])?.clone();
                let right = decode::text_ref(&mut right);

                // TODO: this compares bytes, not Unicode comparisons
                left.cmp(right)
            }
        })
    }

    bin_prim_func!(eq, eq, bool);

    bin_prim_func!(lt, lt, bool);

    bin_prim_func!(lte, le, bool);

    bin_func!(and, bool, &&, bool);

    bin_func!(or, bool, ||, bool);

    pub fn not(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let operand = assume::bool(&args[0])?;
        let res = !operand;
        Ok(Cell::Data(encode(&res)))
    }

    pub fn index(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let input_item_head_bytes = layout_args.next_u32();

        let output_format = assume::bytes(layout_args.next_slice());
        let output_format = lutra_bin::layout::EnumFormat::decode(&output_format).unwrap();

        let [array, position] = assume::exactly_n(args);

        let array = assume::array(array, input_item_head_bytes);
        let position = assume::int64(&position)?;
        let position = position as usize;

        let enum_writer = EnumWriter::new(output_format);

        Ok(Cell::Data(if position >= array.remaining() {
            // None
            enum_writer.write(0, Data::new(vec![]))
        } else {
            // Some
            let item = array.get(position).unwrap();
            enum_writer.write(1, item)
        }))
    }

    pub fn map(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let input_item_head_bytes = layout_args.next_u32();
        let output_item_head_bytes = layout_args.next_u32();
        let output_item_body_ptrs = layout_args.next_slice();

        let [array, func] = assume::exactly_n(args);

        let mut output = ArrayWriter::new(output_item_head_bytes, output_item_body_ptrs);
        for item in assume::array(array, input_item_head_bytes) {
            let cell = Cell::Data(item);

            let value = it.evaluate_func_call(&func, vec![cell])?;

            output.write_item(assume::into_data(value)?);
        }

        Ok(Cell::Data(output.finish()))
    }

    pub fn flat_map(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let input_item_head_bytes = layout_args.next_u32();
        let output_item_head_bytes = layout_args.next_u32();
        let output_item_body_ptrs = layout_args.next_slice();

        let [array, func] = assume::exactly_n(args);

        let mut output = ArrayWriter::new(output_item_head_bytes, output_item_body_ptrs);
        for item in assume::array(array, input_item_head_bytes) {
            let cell = Cell::Data(item);

            let value = it.evaluate_func_call(&func, vec![cell])?;

            for i in assume::array(value, output_item_head_bytes) {
                output.write_item(i);
            }
        }

        Ok(Cell::Data(output.finish()))
    }

    pub fn filter(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let item_head_bytes = layout_args.next_u32();
        let item_body_ptrs = layout_args.next_slice();

        let [array, func] = assume::exactly_n(args);
        let input = assume::array(array, item_head_bytes);

        let mut output = ArrayWriter::new(item_head_bytes, item_body_ptrs);
        for item in input {
            let item_c = Cell::Data(item.clone());

            let condition = it.evaluate_func_call(&func, vec![item_c])?;
            let condition = assume::bool(&condition)?;

            if condition {
                output.write_item(item);
            }
        }

        Ok(Cell::Data(output.finish()))
    }

    pub fn slice(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let item_head_bytes = layout_args.next_u32();
        let item_body_ptrs = layout_args.next_slice();

        let [array, start, end] = assume::exactly_n(args);

        let input = assume::array(array, item_head_bytes);

        // unpack
        let start = assume::int64(&start)?;
        let end = assume::int64(&end)?;

        // convert to absolute
        let start = index_rel_to_abs(start, input.remaining());
        let end = index_rel_to_abs(end, input.remaining());

        // convert to skip & take
        let skip = start;
        let take = end.saturating_sub(skip);

        let mut output = ArrayWriter::new(item_head_bytes, item_body_ptrs);
        for item in input.skip(start).take(take) {
            output.write_item(item);
        }
        Ok(Cell::Data(output.finish()))
    }

    pub fn sort(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let item_head_bytes = layout_args.next_u32();
        let item_body_ptrs = layout_args.next_slice();

        let key_ty = layout_args.next_u32();

        let [array, func] = assume::exactly_n(args);

        let input = assume::array(array, item_head_bytes);

        let mut keys = Vec::with_capacity(input.remaining());
        for (index, item) in input.clone().enumerate() {
            let cell = Cell::Data(item);

            let key = it.evaluate_func_call(&func, vec![cell])?;

            keys.push((key, index));
        }

        keys.sort_by(|(a, _), (b, _)| {
            cmp_raw(key_ty, &[a.clone(), b.clone()]).unwrap_or(Ordering::Equal)
        });

        let mut output = ArrayWriter::new(item_head_bytes, item_body_ptrs);
        for (_key, index) in keys {
            let item = input.get(index).unwrap();
            output.write_item(item);
        }
        Ok(Cell::Data(output.finish()))
    }

    pub fn to_columnar(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let input_item_head_bytes = layout_args.next_u32();
        let fields_offsets = layout_args.next_slice();
        let fields_head_bytes = layout_args.next_slice();

        let [array] = assume::exactly_n(args);
        let input = assume::array(array, input_item_head_bytes);

        // init output arrays
        let mut output_arrays: Vec<ArrayWriter> = fields_head_bytes
            .iter()
            .map(|head_bytes| {
                let body_ptrs = layout_args.next_slice();
                ArrayWriter::new(*head_bytes, body_ptrs)
            })
            .collect();

        // partition input into output arrays
        for item in input {
            let tuple = TupleReader::new(item, fields_offsets.into());

            for (index, output_array) in output_arrays.iter_mut().enumerate() {
                let field = tuple.get_field(index);
                output_array.write_item(field);
            }
        }

        // hard-coded tuple of arrays layout
        let body_ptr = [0_u32]; // arrays has body ptr at 0
        let mut output_fields_layouts = Vec::with_capacity(fields_offsets.len());
        for _ in 0..fields_offsets.len() {
            let head_bytes = 8_u32;
            output_fields_layouts.push((head_bytes, body_ptr.as_slice()))
        }

        // construct the final tuple
        let mut output = TupleWriter::new(output_fields_layouts.into());
        for output_array in output_arrays {
            let array = output_array.finish();
            output.write_field(array);
        }
        Ok(Cell::Data(output.finish()))
    }

    pub fn from_columnar(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let output_head_bytes = layout_args.next_u32();
        let output_body_ptrs = layout_args.next_slice();
        let fields_item_head_bytes = layout_args.next_slice();

        let mut output_fields_layouts = Vec::new();
        for field_head_bytes in fields_item_head_bytes {
            output_fields_layouts.push((*field_head_bytes, layout_args.next_slice()));
        }

        let [columnar] = assume::exactly_n(args);

        // hard-coded tuple of arrays layout
        let mut field_offsets = Vec::with_capacity(fields_item_head_bytes.len());
        for i in 0..fields_item_head_bytes.len() {
            field_offsets.push(8_u32 * i as u32); // array head has 8 bytes
        }

        let input = TupleReader::new(assume::into_data(columnar)?, field_offsets.into());

        // init input array readers
        let mut input_arrays: Vec<ArrayReader<_>> = fields_item_head_bytes
            .iter()
            .enumerate()
            .map(|(i, head_bytes)| ArrayReader::new(input.get_field(i), *head_bytes as usize))
            .collect();

        // init output array
        let mut output = ArrayWriter::new(output_head_bytes, output_body_ptrs);

        // short-circuit empty tuples
        if output_head_bytes == 0 {
            return Ok(Cell::Data(output.finish()));
        }
        'output: loop {
            let mut tuple = TupleWriter::new(Cow::from(&output_fields_layouts));
            for (index, _) in output_fields_layouts.iter().enumerate() {
                if let Some(item) = input_arrays.get_mut(index).unwrap().next() {
                    tuple.write_field(item)
                } else {
                    break 'output;
                }
            }
            output.write_item(tuple.finish());
        }
        Ok(Cell::Data(output.finish()))
    }

    pub fn zip(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let l_head_bytes = layout_args.next_u32();
        let l_body_ptrs = layout_args.next_slice();
        let r_head_bytes = layout_args.next_u32();
        let r_body_ptrs = layout_args.next_slice();

        let output_head_bytes = l_head_bytes + r_head_bytes;
        let mut output_body_ptrs = l_body_ptrs.to_vec();
        output_body_ptrs.extend(r_body_ptrs.iter().map(|p| p + l_head_bytes));

        let output_fields_layouts = [(l_head_bytes, l_body_ptrs), (r_head_bytes, r_body_ptrs)];

        let [left, right] = assume::exactly_n(args);
        let left = ArrayReader::new(assume::into_data(left)?, l_head_bytes as usize);
        let right = ArrayReader::new(assume::into_data(right)?, r_head_bytes as usize);

        let mut output = ArrayWriter::new(output_head_bytes, &output_body_ptrs);
        for (l, r) in left.zip(right) {
            let mut tuple = TupleWriter::new(Cow::from(&output_fields_layouts));
            tuple.write_field(l);
            tuple.write_field(r);

            output.write_item(tuple.finish());
        }
        Ok(Cell::Data(output.finish()))
    }

    pub fn group(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let input_head_bytes = layout_args.next_u32();
        let input_body_ptrs = layout_args.next_slice();
        let output_head_bytes = layout_args.next_u32();
        let output_body_ptrs = layout_args.next_slice();

        let output_field_head_bytes = layout_args.next_slice();
        let mut output_fields_layouts = Vec::new();
        for field_head_bytes in output_field_head_bytes {
            output_fields_layouts.push((*field_head_bytes, layout_args.next_slice()));
        }

        let [input, key_getter] = assume::exactly_n(args);
        let input = ArrayReader::new(assume::into_data(input)?, input_head_bytes as usize);

        let mut partitions: HashMap<Vec<u8>, Vec<Data>> = HashMap::new();
        for item in input {
            let item_cell = Cell::Data(item.clone());

            let key = it.evaluate_func_call(&key_getter, vec![item_cell])?;
            let key = assume::into_data(key)?.chunk().to_vec();

            let partition = partitions.entry(key).or_default();
            partition.push(item);
        }

        // init output array
        let mut output = ArrayWriter::new(output_head_bytes, output_body_ptrs);
        for (key, values) in partitions {
            let mut tuple = TupleWriter::new(Cow::from(&output_fields_layouts));
            tuple.write_field(Data::new(key));

            let mut partition = ArrayWriter::new(input_head_bytes, input_body_ptrs);
            for value in values {
                partition.write_item(value);
            }
            tuple.write_field(partition.finish());

            output.write_item(tuple.finish());
        }
        Ok(Cell::Data(output.finish()))
    }

    pub fn append(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let input_head_bytes = layout_args.next_u32();
        let input_body_ptrs = layout_args.next_slice();

        let [first, second] = assume::exactly_n(args);

        let mut output = ArrayWriter::new(input_head_bytes, input_body_ptrs);

        let first = ArrayReader::new(assume::into_data(first)?, input_head_bytes as usize);
        for item in first {
            output.write_item(item);
        }

        let second = ArrayReader::new(assume::into_data(second)?, input_head_bytes as usize);
        for item in second {
            output.write_item(item);
        }
        Ok(Cell::Data(output.finish()))
    }

    pub fn fold(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let input_head_bytes = layout_args.next_u32();

        let [inputs, initial, operation] = assume::exactly_n(args);

        let inputs = ArrayReader::new(assume::into_data(inputs)?, input_head_bytes as usize);
        let mut acc = initial;
        for input in inputs {
            let args = vec![acc, Cell::Data(input)];
            acc = it.evaluate_func_call(&operation, args)?;
        }
        Ok(acc)
    }

    pub fn scan(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let input_head_bytes = layout_args.next_u32();
        let output_head_bytes = layout_args.next_u32();
        let output_body_ptrs = layout_args.next_slice();

        let [inputs, initial, operation] = assume::exactly_n(args);

        let inputs = ArrayReader::new(assume::into_data(inputs)?, input_head_bytes as usize);
        let mut acc = assume::into_data(initial)?;
        let mut outputs = ArrayWriter::new(output_head_bytes, output_body_ptrs);
        for input in inputs {
            let args = vec![Cell::Data(acc), Cell::Data(input)];
            acc = assume::into_data(it.evaluate_func_call(&operation, args)?)?;
            outputs.write_item(acc.clone());
        }
        Ok(Cell::Data(outputs.finish()))
    }

    pub fn apply_until_empty(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let ty_head_bytes = layout_args.next_u32();
        let ty_body_ptrs = layout_args.next_slice();

        let [initial, function] = assume::exactly_n(args);

        let mut value = initial;
        let mut outputs = ArrayWriter::new(ty_head_bytes, ty_body_ptrs);
        loop {
            let values =
                ArrayReader::new(assume::into_data(value.clone())?, ty_head_bytes as usize);
            if values.remaining() == 0 {
                break;
            }
            for v in values {
                outputs.write_item(v);
            }

            value = it.evaluate_func_call(&function, vec![value])?;
        }
        Ok(Cell::Data(outputs.finish()))
    }

    reduce_func!(
        min,
        decode::int,
        |a, b| if a < b { a } else { b },
        core::convert::identity
    );

    reduce_func!(
        max,
        decode::int,
        |a, b| if a > b { a } else { b },
        core::convert::identity
    );

    macro_rules! sum {
        ($array: expr, $ty: ident) => {{
            let s = $array
                .map(|x| decode::primitive::<$ty>(&x))
                .reduce(|a, b| a + b);
            encode(&s.unwrap_or_default())
        }};
    }

    pub fn sum(
        _: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array] = assume::exactly_n(args);
        let array = assume::array(array, layout_args[0]);
        let ty_prim = decode::ty_primitive(layout_args[1]);

        let res = match ty_prim {
            ir::TyPrimitive::int8 => sum!(array, i8),
            ir::TyPrimitive::int16 => sum!(array, i16),
            ir::TyPrimitive::int32 => sum!(array, i32),
            ir::TyPrimitive::int64 => sum!(array, i64),
            ir::TyPrimitive::uint8 => sum!(array, u8),
            ir::TyPrimitive::uint16 => sum!(array, u16),
            ir::TyPrimitive::uint32 => sum!(array, u32),
            ir::TyPrimitive::uint64 => sum!(array, u64),
            ir::TyPrimitive::float32 => sum!(array, f32),
            ir::TyPrimitive::float64 => sum!(array, f64),
            _ => panic!(),
        };
        Ok(Cell::Data(res))
    }

    pub fn count(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array] = assume::exactly_n(args);
        let array = assume::into_data(array)?;

        let (_offset, len) = lutra_bin::ArrayReader::<&[u8]>::read_head(array.chunk());

        let res = len as i64;
        Ok(Cell::Data(encode(&res)))
    }

    macro_rules! mean {
        ($array: expr, $ty: ident) => {{
            let (sum, count) = $array
                .map(|x| decode::primitive::<$ty>(&x))
                .fold(($ty::default(), 0_u32), |(sum, count), item| {
                    (sum + item, count + 1)
                });
            (sum as f64, count)
        }};
    }

    pub fn mean(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array] = assume::exactly_n(args);
        let array = assume::array(array, layout_args[0]);
        let ty_prim = decode::ty_primitive(layout_args[1]);

        let (sum, count) = match ty_prim {
            ir::TyPrimitive::int8 => mean!(array, i8),
            ir::TyPrimitive::int16 => mean!(array, i16),
            ir::TyPrimitive::int32 => mean!(array, i32),
            ir::TyPrimitive::int64 => mean!(array, i64),
            ir::TyPrimitive::uint8 => mean!(array, u8),
            ir::TyPrimitive::uint16 => mean!(array, u16),
            ir::TyPrimitive::uint32 => mean!(array, u32),
            ir::TyPrimitive::uint64 => mean!(array, u64),
            ir::TyPrimitive::float32 => mean!(array, f32),
            ir::TyPrimitive::float64 => mean!(array, f64),
            _ => panic!(),
        };
        Ok(Cell::Data(encode(&(sum / (count as f64)))))
    }

    pub fn all(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array] = assume::exactly_n(args);
        let mut array = assume::array(array, 1);

        let res = array.all(|x| decode::bool(&x));
        Ok(Cell::Data(encode(&res)))
    }

    pub fn any(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array] = assume::exactly_n(args);
        let mut array = assume::array(array, 1);

        let res = array.any(|x| decode::bool(&x));
        Ok(Cell::Data(encode(&res)))
    }

    pub fn concat_array(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array, separator] = assume::exactly_n(args);

        let array = assume::array(array, 8); // text head = 8
        let separator = assume::text(&separator)?;

        let array: Vec<_> = array.map(|x| decode::text(&x)).collect();
        let res = array.join(&separator);
        Ok(Cell::Data(encode(&res)))
    }

    pub fn lag(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let head_bytes = layout_args.next_u32();
        let body_ptrs = layout_args.next_slice();
        let default_val = assume::bytes(layout_args.next_slice());

        let [array, offset] = assume::exactly_n(args);

        let array = assume::array(array, head_bytes);
        let offset = assume::int64(&offset)? as isize;

        shift(array, offset, head_bytes, body_ptrs, default_val)
    }

    pub fn lead(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let mut layout_args = LayoutArgsReader::new(layout_args);
        let head_bytes = layout_args.next_u32();
        let body_ptrs = layout_args.next_slice();
        let default_val = assume::bytes(layout_args.next_slice());

        let [array, offset] = assume::exactly_n(args);

        let array = assume::array(array, head_bytes);
        let offset = (assume::int64(&offset)? as isize).saturating_neg();

        shift(array, offset, head_bytes, body_ptrs, default_val)
    }

    fn shift(
        array: ArrayReader<Data>,
        offset: isize,
        head_bytes: u32,
        body_ptrs: &[u32],
        default_val: Vec<u8>,
    ) -> Result<Cell, EvalError> {
        tracing::debug!("default val = {default_val:?}");
        let default_val = Data::new(default_val);

        let array_len = array.remaining();

        let n_blanks_before = (offset.max(0) as usize).min(array_len);
        let n_blanks_after = (offset.saturating_neg().max(0) as usize).min(array_len);

        let mut out = ArrayWriter::new(head_bytes, body_ptrs);
        for _ in 0..n_blanks_before {
            out.write_item(default_val.clone());
        }

        let n_copies = array_len.saturating_sub(n_blanks_before + n_blanks_after);
        for item in array.skip(n_blanks_after).take(n_copies) {
            out.write_item(item);
        }

        for _ in 0..n_blanks_after {
            out.write_item(default_val.clone());
        }

        Ok(Cell::Data(out.finish()))
    }

    pub fn row_number(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array] = assume::exactly_n(args);
        let array = assume::into_data(array)?;

        let (_offset, len) = lutra_bin::ArrayReader::<&[u8]>::read_head(array.chunk());

        let mut out = ArrayWriter::new(8, &[]); // uint64 head = 8
        for index in 0..len {
            out.write_item(encode(&(index as u64)));
        }
        Ok(Cell::Data(out.finish()))
    }

    pub fn rolling_mean(
        _it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array, trailing, leading] = assume::exactly_n(args);
        let trailing: u32 = assume::primitive(&trailing)?;
        let leading: u32 = assume::primitive(&leading)?;

        let item_head_bytes = layout_args[0];
        let array = assume::array(array, item_head_bytes);
        let ty_prim = decode::ty_primitive(layout_args[1]);

        match ty_prim {
            ir::TyPrimitive::int8 => rolling_mean_impl::<i8>(array, trailing, leading),
            ir::TyPrimitive::int16 => rolling_mean_impl::<i16>(array, trailing, leading),
            ir::TyPrimitive::int32 => rolling_mean_impl::<i32>(array, trailing, leading),
            ir::TyPrimitive::int64 => rolling_mean_impl::<i64>(array, trailing, leading),
            ir::TyPrimitive::uint8 => rolling_mean_impl::<u8>(array, trailing, leading),
            ir::TyPrimitive::uint16 => rolling_mean_impl::<u16>(array, trailing, leading),
            ir::TyPrimitive::uint32 => rolling_mean_impl::<u32>(array, trailing, leading),
            ir::TyPrimitive::uint64 => rolling_mean_impl::<u64>(array, trailing, leading),
            ir::TyPrimitive::float32 => rolling_mean_impl::<f32>(array, trailing, leading),
            ir::TyPrimitive::float64 => rolling_mean_impl::<f64>(array, trailing, leading),
            _ => unreachable!(),
        }
    }

    fn rolling_mean_impl<T>(
        array: ArrayReader<Data>,
        trailing: u32,
        leading: u32,
    ) -> Result<Cell, EvalError>
    where
        T: Decode + Default + ::std::ops::AddAssign + ::std::ops::SubAssign + Copy + ToF64,
    {
        let array_len = array.remaining();

        let mut iter_add = array.map(|x| decode::primitive::<T>(&x));
        let mut iter_remove = iter_add.clone();

        let mut sum = T::default();
        let mut count: usize = 0;
        let mut out = ArrayWriter::new(f64::head_size().div_ceil(8) as u32, &[]);

        // accumulate leading
        for _ in 0..leading {
            if let Some(l) = iter_add.next() {
                sum += l;
                count += 1;
            }
        }

        // step trough windows that only increase
        for _ in 0..(trailing + 1).min(array_len as u32) {
            if let Some(l) = iter_add.next() {
                sum += l;
                count += 1;
            }
            out.write_item(encode(&(sum.to_f64() / count as f64)));
        }

        // step trough all remaining elements
        for _ in 0..(array_len.saturating_sub(trailing as usize + 1)) {
            if let Some(l) = iter_add.next() {
                sum += l;
                count += 1;
            }
            if let Some(t) = iter_remove.next() {
                sum -= t;
                count -= 1;
            }
            out.write_item(encode(&(sum.to_f64() / count as f64)));
        }
        Ok(Cell::Data(out.finish()))
    }

    fn index_rel_to_abs(index: i64, array_len: usize) -> usize {
        if index < 0 {
            array_len.saturating_sub((-index) as usize)
        } else {
            index as usize
        }
    }

    trait ToF64 {
        fn to_f64(self) -> f64;
    }
    macro_rules! impl_to_f64 {
        ($t: ident) => {
            impl ToF64 for $t {
                fn to_f64(self) -> f64 {
                    self as f64
                }
            }
        };
    }
    impl_to_f64!(i8);
    impl_to_f64!(i16);
    impl_to_f64!(i32);
    impl_to_f64!(i64);
    impl_to_f64!(u8);
    impl_to_f64!(u16);
    impl_to_f64!(u32);
    impl_to_f64!(u64);
    impl_to_f64!(f32);
    impl_to_f64!(f64);
}

pub mod std_text {
    use crate::{Data, EvalError, native::*};
    use lutra_bin::ReaderExt;

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "concat" => &concat,
                "length" => &length,
                "from_ascii" => &from_ascii,
                "join" => &join,

                _ => return None,
            })
        }
    }

    pub fn concat(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [left, right] = assume::exactly_n(args);

        // TODO: string reader
        let mut left = assume::into_data(left)?;
        let left_offset = u32::from_le_bytes(left.chunk().read_const());
        let left_length = u32::from_le_bytes((&left.chunk()[4..8]).read_const());
        left.advance(left_offset as usize);

        let mut right = assume::into_data(right)?;
        let right_offset = u32::from_le_bytes(right.chunk().read_const());
        let right_length = u32::from_le_bytes((&right.chunk()[4..8]).read_const());
        right.advance(right_offset as usize);

        // TODO: string writer
        let mut buf = Vec::with_capacity(8 + left_length as usize + right_length as usize);
        buf.extend(&[8, 0, 0, 0]);
        buf.extend((left_length + right_length).to_le_bytes());
        buf.extend(&left.chunk()[0..left_length as usize]);
        buf.extend(&right.chunk()[0..right_length as usize]);

        Ok(Cell::Data(Data::new(buf)))
    }

    pub fn length(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [text] = assume::exactly_n(args);

        let mut data = assume::into_data(text)?;
        let string = decode::text_ref(&mut data);
        let length = string.chars().count() as u32;

        Ok(Cell::Data(encode(&length)))
    }

    pub fn from_ascii(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [ascii] = assume::exactly_n(args);
        let ascii = assume::primitive::<u8>(&ascii)?;

        let text = char::from(ascii).to_string();
        Ok(Cell::Data(encode(&text)))
    }

    pub fn join(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [parts, sep] = assume::exactly_n(args);
        let mut parts = assume::array(parts, str::head_size().div_ceil(8) as u32);
        let sep = assume::text(&sep)?;

        let joined = match parts.next() {
            None => String::new(),
            Some(mut first) => {
                let (lower, _) = parts.size_hint();

                let mut result = String::with_capacity(sep.len() * lower);
                result.push_str(decode::text_ref(&mut first));
                for mut part in parts {
                    result.push_str(&sep);
                    result.push_str(decode::text_ref(&mut part));
                }
                result
            }
        };
        Ok(Cell::Data(encode(&joined)))
    }
}

pub mod std_math {
    use crate::native::{assume, decode, encode};
    use crate::{Cell, EvalError, Interpreter, NativeModule};
    use lutra_bin::ir;

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "abs" => &abs,
                "pow" => &pow,

                _ => return None,
            })
        }
    }

    fn abs(_it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [arg] = assume::exactly_n(args);

        let ty_prim = decode::ty_primitive(layout_args[0]);

        Ok(Cell::Data(match ty_prim {
            ir::TyPrimitive::int8 => encode(&assume::primitive::<i8>(&arg)?.abs()),
            ir::TyPrimitive::int16 => encode(&assume::primitive::<i16>(&arg)?.abs()),
            ir::TyPrimitive::int32 => encode(&assume::primitive::<i32>(&arg)?.abs()),
            ir::TyPrimitive::int64 => encode(&assume::primitive::<i64>(&arg)?.abs()),
            ir::TyPrimitive::float32 => encode(&assume::primitive::<f32>(&arg)?.abs()),
            ir::TyPrimitive::float64 => encode(&assume::primitive::<f64>(&arg)?.abs()),

            ir::TyPrimitive::uint8
            | ir::TyPrimitive::uint16
            | ir::TyPrimitive::uint32
            | ir::TyPrimitive::uint64 => return Ok(arg),

            _ => return Err(EvalError::BadProgram),
        }))
    }

    pub fn pow(
        _: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let prim = decode::ty_primitive(layout_args[0]);
        Ok(Cell::Data(match prim {
            ir::TyPrimitive::int8 => {
                let (left, right) = assume::primitive2::<i8>(&args)?;
                let res = left.pow(right as u32);
                encode::<i8>(&res)
            }
            ir::TyPrimitive::int16 => {
                let (left, right) = assume::primitive2::<i16>(&args)?;
                let res = left.pow(right as u32);
                encode::<i16>(&res)
            }
            ir::TyPrimitive::int32 => {
                let (left, right) = assume::primitive2::<i32>(&args)?;
                let res = left.pow(right as u32);
                encode::<i32>(&res)
            }
            ir::TyPrimitive::int64 => {
                let (left, right) = assume::primitive2::<i64>(&args)?;
                let res = left.pow(right as u32);
                encode::<i64>(&res)
            }
            ir::TyPrimitive::uint8 => {
                let (left, right) = assume::primitive2::<u8>(&args)?;
                let res = left.pow(right as u32);
                encode::<u8>(&res)
            }
            ir::TyPrimitive::uint16 => {
                let (left, right) = assume::primitive2::<u16>(&args)?;
                let res = left.pow(right as u32);
                encode::<u16>(&res)
            }
            ir::TyPrimitive::uint32 => {
                let (left, right) = assume::primitive2::<u32>(&args)?;
                let res = left.pow(right);
                encode::<u32>(&res)
            }
            ir::TyPrimitive::uint64 => {
                let (left, right) = assume::primitive2::<u64>(&args)?;
                let res = left.pow(right as u32);
                encode::<u64>(&res)
            }
            ir::TyPrimitive::float32 => {
                let (left, right) = assume::primitive2::<f32>(&args)?;
                let res = left.powf(right);
                encode::<f32>(&res)
            }
            ir::TyPrimitive::float64 => {
                let (left, right) = assume::primitive2::<f64>(&args)?;
                let res = left.powf(right);
                encode::<f64>(&res)
            }
            _ => panic!(),
        }))
    }
}

pub mod std_fs {
    use ::std::{fs, io, path};

    use arrow::array::RecordBatchReader;
    use lutra_bin::{Decode, ir};
    use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
    use parquet::arrow::arrow_writer::ArrowWriter;

    use crate::{Data, EvalError, native::*};

    impl Interpreter {
        fn resolve_path(&self, path: &str) -> Result<path::PathBuf, EvalError> {
            let Some(file_system_root) = &self.file_system else {
                return Err(EvalError::ExternalError(
                    "lutra-interpreter was not provided with file-system access".into(),
                ));
            };

            let resolved = file_system_root.join(path);
            if !resolved.starts_with(file_system_root) {
                return Err(EvalError::ExternalError(format!("invalid path: {path}")));
            }
            Ok(resolved)
        }
    }

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "read_parquet" => &read_parquet,
                "write_parquet" => &write_parquet,

                _ => return None,
            })
        }
    }

    pub fn read_parquet(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        // unpack args
        let [file_path] = assume::exactly_n(args);
        let file_path = assume::text(&file_path)?;
        let file_path = it.resolve_path(&file_path)?;

        // decode item ty from layout args
        let mut layout_args = assume::LayoutArgsReader::new(layout_args);
        let ty_item = assume::bytes(layout_args.next_slice());
        let ty_item = ir::Ty::decode(&ty_item).map_err(|_| EvalError::BadProgram)?;

        // init parquet reader
        let file = match fs::File::open(&file_path) {
            Ok(file) => file,
            Err(e) => {
                return Err(EvalError::ExternalError(format!("read_parquet: {e}")));
            }
        };
        let builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();

        let reader = builder.build().unwrap();

        crate::arrow::validate_schema(&reader.schema(), &ty_item).unwrap();

        let data =
            crate::arrow::arrow_to_lutra(reader, &ty_item).map_err(|_| EvalError::BadProgram)?;

        Ok(Cell::Data(data))
    }

    pub fn write_parquet(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        // unpack args
        let [data, file_path] = assume::exactly_n(args);

        let file_path = assume::text(&file_path)?;
        let file_path = it.resolve_path(&file_path)?;

        let data = assume::into_data(data)?;

        // decode item ty from layout args
        let mut layout_args = assume::LayoutArgsReader::new(layout_args);
        let ty_item = assume::bytes(layout_args.next_slice());
        let ty_item = ir::Ty::decode(&ty_item).map_err(|_| EvalError::BadProgram)?;

        // convert lutra to arrow
        let data = crate::arrow::lutra_to_arrow(data, &ty_item);

        // write to parquet
        let file = match fs::File::create(&file_path) {
            Ok(file) => file,
            Err(e) => {
                return Err(EvalError::ExternalError(format!("write_parquet: {e}")));
            }
        };
        let mut writer = io::BufWriter::new(file);
        let mut builder = ArrowWriter::try_new(&mut writer, data.schema(), None).unwrap();
        builder.write(&data).unwrap();
        builder.finish().unwrap();

        Ok(Cell::Data(Data::new(vec![])))
    }
}

pub mod interpreter {
    use crate::{EvalError, native::*};

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "version" => &version,
                _ => return None,
            })
        }
    }

    fn version(
        _it: &mut Interpreter,
        _layout_args: &[u32],
        _args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        Ok(Cell::Data(encode("lutra-interpreter 0.0.1")))
    }
}

mod assume {
    use super::decode;
    use crate::{Data, EvalError, interpreter::Cell};
    use lutra_bin::{ArrayReader, Decode};

    pub fn into_data(cell: Cell) -> Result<Data, EvalError> {
        match cell {
            Cell::Data(val) => Ok(val),
            Cell::Function(..) => Err(EvalError::BadProgram),
            Cell::FunctionNative(..) => Err(EvalError::BadProgram),
            Cell::Vacant => Err(EvalError::Bug),
        }
    }

    pub fn as_data(cell: &Cell) -> Result<&Data, EvalError> {
        match cell {
            Cell::Data(val) => Ok(val),
            Cell::Function(..) => Err(EvalError::BadProgram),
            Cell::FunctionNative(..) => Err(EvalError::BadProgram),
            Cell::Vacant => Err(EvalError::Bug),
        }
    }

    pub fn exactly_n<const N: usize>(args: Vec<Cell>) -> [Cell; N] {
        let mut res = [const { Cell::Vacant }; N];
        assert_eq!(args.len(), N);
        for (i, cell) in args.into_iter().enumerate() {
            res[i] = cell;
        }
        res
    }

    pub fn primitive<T: Decode>(cell: &Cell) -> Result<T, EvalError> {
        Ok(decode::primitive::<T>(as_data(cell)?))
    }

    pub fn primitive2<T: Decode>(cells: &[Cell]) -> Result<(T, T), EvalError> {
        Ok((primitive(&cells[0])?, primitive(&cells[1])?))
    }

    pub fn int64(cell: &Cell) -> Result<i64, EvalError> {
        Ok(decode::primitive(as_data(cell)?))
    }

    pub fn bool(cell: &Cell) -> Result<bool, EvalError> {
        Ok(decode::primitive(as_data(cell)?))
    }

    pub fn text(cell: &Cell) -> Result<String, EvalError> {
        Ok(decode::text(as_data(cell)?))
    }

    pub fn array(cell: Cell, item_head_bytes: u32) -> ArrayReader<Data> {
        ArrayReader::new(into_data(cell).unwrap(), item_head_bytes as usize)
    }

    pub struct LayoutArgsReader<'t> {
        layout_args: &'t [u32],
    }

    impl<'t> LayoutArgsReader<'t> {
        pub fn new(layout_args: &'t [u32]) -> Self {
            Self { layout_args }
        }

        pub fn next_u32(&mut self) -> u32 {
            let r = self.layout_args[0];
            self.layout_args = &self.layout_args[1..];
            r
        }

        pub fn next_slice(&mut self) -> &'t [u32] {
            let len = self.layout_args[0] as usize;
            let r = &self.layout_args[1..len + 1];
            self.layout_args = &self.layout_args[len + 1..];
            r
        }
    }

    pub fn bytes(args: &[u32]) -> Vec<u8> {
        let len = args[0] as usize;
        let mut r = Vec::with_capacity(args.len() * (u32::BITS / u8::BITS) as usize);
        for arg in &args[1..] {
            r.extend(arg.to_le_bytes());
        }
        r.truncate(len);
        r
    }
}

mod decode {
    use lutra_bin::{ArrayReader, Decode, ir};

    use crate::Data;

    pub fn primitive<T: Decode>(data: &Data) -> T {
        T::decode(data.chunk()).unwrap()
    }

    pub fn int(data: &Data) -> i64 {
        i64::decode(data.chunk()).unwrap()
    }

    pub fn bool(data: &Data) -> bool {
        bool::decode(data.chunk()).unwrap()
    }

    pub fn text(data: &Data) -> String {
        String::decode(&data.flatten()).unwrap()
    }

    pub fn text_ref(data: &mut Data) -> &str {
        let (offset, len) = ArrayReader::<&[u8]>::read_head(data.chunk());
        data.advance(offset);

        str::from_utf8(&data.chunk()[..len]).unwrap()
    }

    pub fn ty_primitive(ty_arg: u32) -> ir::TyPrimitive {
        // keep in sync with generated ir code!
        match ty_arg.to_be() {
            0 => ir::TyPrimitive::bool,
            1 => ir::TyPrimitive::int8,
            2 => ir::TyPrimitive::int16,
            3 => ir::TyPrimitive::int32,
            4 => ir::TyPrimitive::int64,
            5 => ir::TyPrimitive::uint8,
            6 => ir::TyPrimitive::uint16,
            7 => ir::TyPrimitive::uint32,
            8 => ir::TyPrimitive::uint64,
            9 => ir::TyPrimitive::float32,
            10 => ir::TyPrimitive::float64,
            11 => ir::TyPrimitive::text,
            _ => unreachable!(),
        }
    }
}

pub fn encode<T: Encode + Layout + ?Sized>(value: &T) -> Data {
    Data::new(value.encode())
}

pub fn encode_cell<T: Encode + Layout + ?Sized>(value: &T) -> Cell {
    Cell::Data(encode(value))
}
