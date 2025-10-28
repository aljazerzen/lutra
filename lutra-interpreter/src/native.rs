use lutra_bin::{Encode, Layout};

use crate::interpreter::{Cell, Interpreter};
use crate::{Data, NativeModule};

pub mod std {
    use ::std::{borrow::Cow, collections::HashMap};

    use crate::{ArrayWriter, Data, EnumWriter, EvalError, TupleWriter, native::*};
    use assume::LayoutArgsReader;
    use lutra_bin::{ArrayReader, Decode, TupleReader, ir};

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "to_int8" => &Self::to_int8,
                "to_int16" => &Self::to_int16,
                "to_int32" => &Self::to_int32,
                "to_int64" => &Self::to_int64,
                "to_uint8" => &Self::to_uint8,
                "to_uint16" => &Self::to_uint16,
                "to_uint32" => &Self::to_uint32,
                "to_uint64" => &Self::to_uint64,
                "to_float32" => &Self::to_float32,
                "to_float64" => &Self::to_float64,

                "mul" => &Self::mul,
                "div" => &Self::div,
                "mod" => &Self::r#mod,
                "add" => &Self::add,
                "sub" => &Self::sub,
                "eq" => &Self::eq,
                "ne" => &Self::ne,
                "gt" => &Self::gt,
                "lt" => &Self::lt,
                "gte" => &Self::gte,
                "lte" => &Self::lte,
                "and" => &Self::and,
                "or" => &Self::or,
                "neg" => &Self::neg,
                "not" => &Self::not,

                "index" => &Self::index,
                "map" => &Self::map,
                "flat_map" => &Self::flat_map,
                "filter" => &Self::filter,
                "slice" => &Self::slice,
                "sort" => &Self::sort,
                "to_columnar" => &Self::to_columnar,
                "from_columnar" => &Self::from_columnar,
                "group" => &Self::group,
                "append" => &Self::append,

                "min" => &Self::min,
                "max" => &Self::max,
                "sum" => &Self::sum,
                "average" => &Self::average,
                "all" => &Self::all,
                "any" => &Self::any,
                "contains" => &Self::contains,
                "concat_array" => &Self::concat_array,
                "count" => &Self::count,

                "lag" => &Self::lag,
                "lead" => &Self::lead,
                "row_number" => &Self::row_number,

                _ => return None,
            })
        }
    }

    macro_rules! reduce_func {
        ($name: ident, $item_decode: path, $reduce: expr, $default: literal) => {
            pub fn $name(
                _: &mut Interpreter,
                layout_args: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let [array] = assume::exactly_n(args);
                let array = assume::array(array, layout_args[0]);

                let res = array
                    .map(|x| $item_decode(&x))
                    .reduce($reduce)
                    .unwrap_or($default);

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
                let input_ty = layout_args[0].to_be_bytes();
                let input_ty = ir::TyPrimitive::decode(&input_ty).unwrap();

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

    macro_rules! bin_op {
        ($args: ident, $prim: ident, $op: tt, unchanged) => {
            bin_op!($args, $prim, $op, $prim)
        };

        ($args: ident, $prim: ty, $op: tt, $res_ty: ty) => {
            {
                let left = assume::primitive::<$prim>(&$args[0])?;
                let right = assume::primitive::<$prim>(&$args[1])?;
                let res = left $op right;
                Cell::Data(encode::<$res_ty>(&res))
            }
        };
    }

    macro_rules! bin_func {
        ($name: ident, $args_ty: ty, $op: tt, $res_ty: ty) => {
            pub fn $name(
                _: &mut Interpreter,
                _layout_args: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                Ok(bin_op!(args, $args_ty, $op, $res_ty))
            }
        };
    }

    macro_rules! bin_num_func {
        ($name: ident, $op: tt, $res_ty: ident) => {
            pub fn $name(
                _: &mut Interpreter,
                layout_args: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let prim_set = layout_args[0].to_be_bytes();
                let prim_set = ir::TyPrimitive::decode(&prim_set).unwrap();

                Ok(match prim_set {
                    ir::TyPrimitive::int8 => bin_op!(args, i8, $op, $res_ty),
                    ir::TyPrimitive::int16 => bin_op!(args, i16, $op, $res_ty),
                    ir::TyPrimitive::int32 => bin_op!(args, i32, $op, $res_ty),
                    ir::TyPrimitive::int64 => bin_op!(args, i64, $op, $res_ty),
                    ir::TyPrimitive::uint8 => bin_op!(args, u8, $op, $res_ty),
                    ir::TyPrimitive::uint16 => bin_op!(args, u16, $op, $res_ty),
                    ir::TyPrimitive::uint32 => bin_op!(args, u32, $op, $res_ty),
                    ir::TyPrimitive::uint64 => bin_op!(args, u64, $op, $res_ty),
                    ir::TyPrimitive::float32 => bin_op!(args, f32, $op, $res_ty),
                    ir::TyPrimitive::float64 => bin_op!(args, f64, $op, $res_ty),
                    _ => panic!(),
                })
            }
        };
    }

    macro_rules! bin_prim_func {
        ($name: ident, $op: tt, $res_ty: ident) => {
            pub fn $name(
                _: &mut Interpreter,
                layout_args: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let prim_set = layout_args[0].to_be_bytes();
                let prim_set = ir::TyPrimitive::decode(&prim_set).unwrap();

                Ok(match prim_set {
                    ir::TyPrimitive::bool => bin_op!(args, bool, $op, $res_ty),
                    ir::TyPrimitive::int8 => bin_op!(args, i8, $op, $res_ty),
                    ir::TyPrimitive::int16 => bin_op!(args, i16, $op, $res_ty),
                    ir::TyPrimitive::int32 => bin_op!(args, i32, $op, $res_ty),
                    ir::TyPrimitive::int64 => bin_op!(args, i64, $op, $res_ty),
                    ir::TyPrimitive::uint8 => bin_op!(args, u8, $op, $res_ty),
                    ir::TyPrimitive::uint16 => bin_op!(args, u16, $op, $res_ty),
                    ir::TyPrimitive::uint32 => bin_op!(args, u32, $op, $res_ty),
                    ir::TyPrimitive::uint64 => bin_op!(args, u64, $op, $res_ty),
                    ir::TyPrimitive::float32 => bin_op!(args, f32, $op, $res_ty),
                    ir::TyPrimitive::float64 => bin_op!(args, f64, $op, $res_ty),
                    ir::TyPrimitive::text => {
                        let left = assume::text(&args[0])?;
                        let right = assume::text(&args[1])?;
                        let res = left $op right;
                        Cell::Data(encode::<$res_ty>(&res))
                    },
                })
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

    impl Module {
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

        bin_num_func!(mul, *, unchanged);

        bin_num_func!(div, /, unchanged);

        bin_num_func!(r#mod, %, unchanged);

        bin_num_func!(add, +, unchanged);

        bin_num_func!(sub, -, unchanged);

        pub fn neg(
            _: &mut Interpreter,
            layout_args: &[u32],
            args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            let prim_set = layout_args[0].to_be_bytes();
            let prim_set = ir::TyPrimitive::decode(&prim_set).unwrap();
            Ok(match prim_set {
                ir::TyPrimitive::int8 => neg_arg!(i8, args),
                ir::TyPrimitive::int16 => neg_arg!(i16, args),
                ir::TyPrimitive::int32 => neg_arg!(i32, args),
                ir::TyPrimitive::int64 => neg_arg!(i64, args),
                ir::TyPrimitive::float32 => neg_arg!(f32, args),
                ir::TyPrimitive::float64 => neg_arg!(f64, args),
                _ => panic!(),
            })
        }

        bin_prim_func!(eq, ==, bool);

        bin_prim_func!(ne, !=, bool);

        bin_num_func!(gt, >, bool);

        bin_num_func!(lt, <, bool);

        bin_num_func!(gte, >=, bool);

        bin_num_func!(lte, <=, bool);

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

            let [array, func] = assume::exactly_n(args);

            let input = assume::array(array, item_head_bytes);

            let mut keys = Vec::with_capacity(input.remaining());
            for (index, item) in input.clone().enumerate() {
                let cell = Cell::Data(item);

                let key = it.evaluate_func_call(&func, vec![cell])?;
                let key = assume::int64(&key)?;

                keys.push((key, index));
            }
            keys.sort();

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

        reduce_func!(min, decode::int, |a, b| if a < b { a } else { b }, 0);

        reduce_func!(max, decode::int, |a, b| if a > b { a } else { b }, 0);

        reduce_func!(sum, decode::int, |a, b| a + b, 0);

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

        pub fn average(
            _it: &mut Interpreter,
            layout_args: &[u32],
            args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            let [array] = assume::exactly_n(args);
            let array = assume::array(array, layout_args[0]);

            let (sum, count) = array
                .map(|x| decode::int(&x))
                .fold((0, 0), |(sum, count), item| (sum + item, count + 1));

            let res = if count == 0 {
                0.0
            } else {
                sum as f64 / count as f64
            };
            Ok(Cell::Data(encode(&res)))
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

        pub fn contains(
            _it: &mut Interpreter,
            layout_args: &[u32],
            args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            let [array, item] = assume::exactly_n(args);
            let array = assume::array(array, layout_args[0]);
            let item = assume::int64(&item)?;

            let res = array.into_iter().any(|x| decode::int(&x) == item);
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

            Self::shift(array, offset, head_bytes, body_ptrs, default_val)
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

            Self::shift(array, offset, head_bytes, body_ptrs, default_val)
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
    }

    fn index_rel_to_abs(index: i64, array_len: usize) -> usize {
        if index < 0 {
            array_len.saturating_sub((-index) as usize)
        } else {
            index as usize
        }
    }
}

pub mod std_text {
    use crate::{Data, EvalError, native::*};
    use lutra_bin::ReaderExt;

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "concat" => &Self::concat,
                "length" => &Self::length,

                _ => return None,
            })
        }
    }

    impl Module {
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
    }
}

pub mod std_math {
    use crate::native::{assume, encode};
    use crate::{Cell, EvalError, Interpreter, NativeModule};
    use lutra_bin::{Decode, ir};

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "abs" => &Self::abs,

                _ => return None,
            })
        }
    }

    impl Module {
        fn abs(
            _it: &mut Interpreter,
            layout_args: &[u32],
            args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            let [arg] = assume::exactly_n(args);

            let prim = layout_args[0].to_be_bytes();
            let prim = ir::TyPrimitive::decode(&prim).unwrap();

            Ok(Cell::Data(match prim {
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
                return Err(EvalError::ExternalError(format!(
                    "lutra-interpreter was not provided with file-system access"
                )));
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
                "read_parquet" => &Self::read_parquet,
                "write_parquet" => &Self::write_parquet,

                _ => return None,
            })
        }
    }

    impl Module {
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

            let data = crate::arrow::arrow_to_lutra(reader, &ty_item)
                .map_err(|_| EvalError::BadProgram)?;

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
}

pub mod interpreter {
    use crate::{EvalError, native::*};

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "version" => &Self::version,
                _ => return None,
            })
        }
    }

    impl Module {
        fn version(
            _it: &mut Interpreter,
            _layout_args: &[u32],
            _args: Vec<Cell>,
        ) -> Result<Cell, EvalError> {
            Ok(Cell::Data(encode("lutra-interpreter 0.0.1")))
        }
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

    #[allow(dead_code)]
    pub fn uint32(cell: Cell) -> u32 {
        decode::uint32(&into_data(cell).unwrap())
    }

    #[allow(dead_code)]
    pub fn array_uint32(cell: Cell) -> Vec<u32> {
        array(cell, 4).map(|x| decode::uint32(&x)).collect()
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
    use lutra_bin::{ArrayReader, Decode};

    use crate::Data;

    pub fn primitive<T: Decode>(data: &Data) -> T {
        T::decode(data.chunk()).unwrap()
    }

    pub fn int(data: &Data) -> i64 {
        i64::decode(data.chunk()).unwrap()
    }

    #[allow(dead_code)]
    pub fn uint32(data: &Data) -> u32 {
        u32::decode(data.chunk()).unwrap()
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
}

pub fn encode<T: Encode + Layout + ?Sized>(value: &T) -> Data {
    Data::new(value.encode())
}
