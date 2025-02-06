use lutra_bin::bytes;
use lutra_bin::{Encode, Layout};

use crate::interpreter::{Cell, Interpreter};
use crate::NativeModule;

pub mod std {
    use ::std::borrow::Cow;

    use crate::native::*;
    use assume::LayoutArgsReader;
    use lutra_bin::{ArrayReader, ArrayWriter, TupleReader, TupleWriter};

    pub const MODULE: Module = Module;

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> crate::interpreter::NativeFunction {
            match id {
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
                "filter" => &Self::filter,
                "slice" => &Self::slice,
                "sort" => &Self::sort,
                "to_columnar" => &Self::to_columnar,
                "from_columnar" => &Self::from_columnar,

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

                _ => panic!(),
            }
        }
    }

    macro_rules! bin_func {
        ($name: ident, $left_assume: path, $right_assume: path, $op: tt) => {
            pub fn $name(_: &mut Interpreter, _layout_args: &[u32], args: Vec<Cell>) -> Cell {
                let left = $left_assume(&args[0]);
                let right = $right_assume(&args[1]);

                let res = left $op right;
                Cell::Data(encode(&res))
            }
        };
    }

    macro_rules! un_func {
        ($name: ident, $assume: path, $op: tt) => {
            pub fn $name(_: &mut Interpreter, _layout_args: &[u32], args: Vec<Cell>) -> Cell {
                let operand = $assume(&args[0]);
                let res = $op operand;
                Cell::Data(encode(&res))
            }
        };
    }

    macro_rules! reduce_func {
        ($name: ident, $item_decode: path, $reduce: expr, $default: literal) => {
            pub fn $name(_: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
                let [array] = assume::args(args);
                let array = assume::array(array, layout_args[0]);

                let res = array
                    .map(|x| $item_decode(&x))
                    .reduce($reduce)
                    .unwrap_or($default);

                Cell::Data(encode(&res))
            }
        };
    }

    impl Module {
        bin_func!(mul, assume::int, assume::int, *);

        bin_func!(div, assume::int, assume::int, /);

        bin_func!(r#mod, assume::int, assume::int, %);

        bin_func!(add, assume::int, assume::int, +);

        bin_func!(sub, assume::int, assume::int, -);

        bin_func!(eq, assume::int, assume::int, ==);

        bin_func!(ne, assume::int, assume::int, !=);

        bin_func!(gt, assume::int, assume::int, >);

        bin_func!(lt, assume::int, assume::int, <);

        bin_func!(gte, assume::int, assume::int, >=);

        bin_func!(lte, assume::int, assume::int, <=);

        bin_func!(and, assume::bool, assume::bool, &&);

        bin_func!(or, assume::bool, assume::bool, ||);

        un_func!(not, assume::bool, !);

        un_func!(neg, assume::int, -);

        pub fn index(_it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let input_item_head_bytes = layout_args[0];

            let [array, position] = assume::args(args);

            let array = assume::array(array, input_item_head_bytes);
            let position = assume::int(&position);

            let item = array.get(position as usize).unwrap();
            Cell::Data(item)
        }

        pub fn map(it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let mut layout_args = LayoutArgsReader::new(layout_args);
            let input_item_head_bytes = layout_args.next_u32();
            let output_item_head_bytes = layout_args.next_u32();
            let output_item_body_ptrs = layout_args.next_slice();

            let [array, func] = assume::args(args);

            let mut output = ArrayWriter::new(output_item_head_bytes, output_item_body_ptrs);
            for item in assume::array(array, input_item_head_bytes) {
                let cell = Cell::Data(item);

                let value = it.evaluate_func_call(&func, vec![cell]);

                output.write_item(assume::into_value(value));
            }

            Cell::Data(output.finish())
        }

        pub fn filter(it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let mut layout_args = LayoutArgsReader::new(layout_args);
            let item_head_bytes = layout_args.next_u32();
            let item_body_ptrs = layout_args.next_slice();

            let [array, func] = assume::args(args);
            let input = assume::array(array, item_head_bytes);

            let mut output = ArrayWriter::new(item_head_bytes, item_body_ptrs);
            for item in input {
                let item_c = Cell::Data(item.clone());

                let condition = it.evaluate_func_call(&func, vec![item_c]);
                let condition = assume::bool(&condition);

                if condition {
                    output.write_item(item);
                }
            }

            Cell::Data(output.finish())
        }

        pub fn slice(_it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let mut layout_args = LayoutArgsReader::new(layout_args);
            let item_head_bytes = layout_args.next_u32();
            let item_body_ptrs = layout_args.next_slice();

            let [array, start, end] = assume::args(args);

            let input = assume::array(array, item_head_bytes);

            // unpack
            let start = assume::int(&start);
            let end = assume::int(&end);

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
            Cell::Data(output.finish())
        }

        pub fn sort(it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let mut layout_args = LayoutArgsReader::new(layout_args);
            let item_head_bytes = layout_args.next_u32();
            let item_body_ptrs = layout_args.next_slice();

            let [array, func] = assume::args(args);

            let input = assume::array(array, item_head_bytes);

            let mut keys = Vec::with_capacity(input.remaining());
            for (index, item) in input.clone().enumerate() {
                let cell = Cell::Data(item);

                let key = it.evaluate_func_call(&func, vec![cell]);
                let key = assume::int(&key);

                keys.push((key, index));
            }
            keys.sort();

            let mut output = ArrayWriter::new(item_head_bytes, item_body_ptrs);
            for (_key, index) in keys {
                let item = input.get(index).unwrap();
                output.write_item(item);
            }
            Cell::Data(output.finish())
        }

        pub fn to_columnar(_it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let mut layout_args = LayoutArgsReader::new(layout_args);
            let input_item_head_bytes = layout_args.next_u32();
            let fields_offsets = layout_args.next_slice();
            let fields_head_bytes = layout_args.next_slice();

            let [array] = assume::args(args);
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
                let tuple = TupleReader::new(&item, fields_offsets.into());

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
            Cell::Data(output.finish())
        }

        pub fn from_columnar(_it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let mut layout_args = LayoutArgsReader::new(layout_args);
            let output_head_bytes = layout_args.next_u32();
            let output_body_ptrs = layout_args.next_slice();
            let fields_item_head_bytes = layout_args.next_slice();

            let mut output_fields_layouts = Vec::new();
            for field_head_bytes in fields_item_head_bytes {
                output_fields_layouts.push((*field_head_bytes, layout_args.next_slice()));
            }

            let [columnar] = assume::args(args);

            // hard-coded tuple of arrays layout
            let mut field_offsets = Vec::with_capacity(fields_item_head_bytes.len());
            for i in 0..fields_item_head_bytes.len() {
                field_offsets.push(8_u32 * i as u32); // array head has 8 bytes
            }

            let input = TupleReader::new(assume::as_value(&columnar), field_offsets.into());

            // init input array readers
            let mut input_arrays: Vec<ArrayReader> = fields_item_head_bytes
                .iter()
                .enumerate()
                .map(|(i, head_bytes)| ArrayReader::new(input.get_field(i), *head_bytes as usize))
                .collect();

            // init output array
            let mut output = ArrayWriter::new(output_head_bytes, output_body_ptrs);

            // short-circuit empty tuples
            if output_head_bytes == 0 {
                return Cell::Data(output.finish());
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
            Cell::Data(output.finish())
        }

        reduce_func!(min, decode::int, |a, b| if a < b { a } else { b }, 0);

        reduce_func!(max, decode::int, |a, b| if a > b { a } else { b }, 0);

        reduce_func!(sum, decode::int, |a, b| a + b, 0);

        pub fn count(_it: &mut Interpreter, _layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let [array] = assume::args(args);
            let array = assume::into_value(array);

            let (_offset, len) = lutra_bin::ArrayReader::read_head(array.slice(8));

            let res = len as i64;
            Cell::Data(encode(&res))
        }

        pub fn average(_it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let [array] = assume::args(args);
            let array = assume::array(array, layout_args[0]);

            let (sum, count) = array
                .map(|x| decode::int(&x))
                .fold((0, 0), |(sum, count), item| (sum + item, count + 1));

            let res = if count == 0 {
                0.0
            } else {
                sum as f64 / count as f64
            };
            Cell::Data(encode(&res))
        }

        pub fn all(_it: &mut Interpreter, _layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let [array] = assume::args(args);
            let mut array = assume::array(array, 1);

            let res = array.all(|x| decode::bool(&x));
            Cell::Data(encode(&res))
        }

        pub fn any(_it: &mut Interpreter, _layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let [array] = assume::args(args);
            let mut array = assume::array(array, 1);

            let res = array.any(|x| decode::bool(&x));
            Cell::Data(encode(&res))
        }

        pub fn contains(_it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let [array, item] = assume::args(args);
            let array = assume::array(array, layout_args[0]);
            let item = assume::int(&item);

            let res = array.into_iter().any(|x| decode::int(&x) == item);
            Cell::Data(encode(&res))
        }

        pub fn concat_array(_it: &mut Interpreter, _layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let [array, separator] = assume::args(args);

            let array = assume::array(array, 8); // text head = 8
            let separator = assume::text(&separator);

            let array: Vec<_> = array.map(|x| decode::text(&x)).collect();
            let res = array.join(&separator);
            Cell::Data(encode(&res))
        }

        pub fn lag(_it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let mut layout_args = LayoutArgsReader::new(layout_args);
            let head_bytes = layout_args.next_u32();
            let body_ptrs = layout_args.next_slice();

            let [array, offset] = assume::args(args);

            let array = assume::array(array, head_bytes);
            let offset = assume::int(&offset).max(0) as usize;

            let mut out = ArrayWriter::new(head_bytes, body_ptrs);

            let n_blanks = offset.min(array.remaining());
            for _ in 0..n_blanks {
                out.write_item(encode(&0_i64));
            }

            let n_copies = array.remaining().saturating_sub(offset);
            for item in array.take(n_copies) {
                out.write_item(item);
            }

            Cell::Data(out.finish())
        }

        pub fn lead(_it: &mut Interpreter, layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let mut layout_args = LayoutArgsReader::new(layout_args);
            let head_bytes = layout_args.next_u32();
            let body_ptrs = layout_args.next_slice();

            let [array, offset] = assume::args(args);

            let array = assume::array(array, head_bytes);
            let offset = assume::int(&offset).max(0) as usize;

            let array_len = array.remaining();

            let mut out = ArrayWriter::new(head_bytes, body_ptrs);

            for item in array.skip(offset) {
                out.write_item(item);
            }

            let n_blanks = offset.min(array_len);
            for _ in 0..n_blanks {
                out.write_item(encode(&0_i64));
            }

            Cell::Data(out.finish())
        }

        pub fn row_number(_it: &mut Interpreter, _layout_args: &[u32], args: Vec<Cell>) -> Cell {
            let [array] = assume::args(args);
            let array = assume::into_value(array);

            let (_offset, len) = lutra_bin::ArrayReader::read_head(array.slice(8));

            let mut out = ArrayWriter::new(8, &[]); // uint64 head = 8
            for index in 0..len {
                out.write_item(encode(&(index as u64)));
            }
            Cell::Data(out.finish())
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

pub mod interpreter {
    use crate::native::*;

    pub const MODULE: Module = Module;
    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> crate::interpreter::NativeFunction {
            match id {
                "version" => &Self::version,
                _ => panic!(),
            }
        }
    }

    impl Module {
        fn version(_it: &mut Interpreter, _layout_args: &[u32], _args: Vec<Cell>) -> Cell {
            Cell::Data(encode(&"lutra-runtime 0.0.1".to_string()))
        }
    }
}

mod assume {
    use super::decode;
    use crate::interpreter::Cell;
    use lutra_bin::ArrayReader;

    pub fn into_value(cell: Cell) -> lutra_bin::Data {
        match cell {
            Cell::Data(val) => val,
            Cell::Function(..) => panic!(),
            Cell::FunctionNative(..) => panic!(),
            Cell::Vacant => panic!(),
        }
    }

    pub fn as_value(cell: &Cell) -> &lutra_bin::Data {
        match cell {
            Cell::Data(val) => val,
            Cell::Function(..) => panic!(),
            Cell::FunctionNative(..) => panic!(),
            Cell::Vacant => panic!(),
        }
    }

    pub fn args<const N: usize>(args: Vec<Cell>) -> [Cell; N] {
        let mut res = [const { Cell::Vacant }; N];
        assert_eq!(args.len(), N);
        for (i, cell) in args.into_iter().enumerate() {
            res[i] = cell;
        }
        res
    }

    pub fn int(cell: &Cell) -> i64 {
        decode::int(as_value(cell))
    }

    pub fn bool(cell: &Cell) -> bool {
        decode::bool(as_value(cell))
    }

    pub fn text(cell: &Cell) -> String {
        decode::text(as_value(cell))
    }

    pub fn array(cell: Cell, item_head_bytes: u32) -> ArrayReader {
        ArrayReader::new(into_value(cell), item_head_bytes as usize)
    }

    #[allow(dead_code)]
    pub fn uint32(cell: Cell) -> u32 {
        decode::uint32(&into_value(cell))
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
}

mod decode {
    use lutra_bin::{Data, Decode};

    pub fn int(data: &Data) -> i64 {
        i64::decode(data.slice(8)).unwrap()
    }

    #[allow(dead_code)]
    pub fn uint32(data: &Data) -> u32 {
        u32::decode(data.slice(4)).unwrap()
    }

    pub fn bool(data: &Data) -> bool {
        bool::decode(data.slice(1)).unwrap()
    }

    pub fn text(data: &Data) -> String {
        // TODO: string reader
        String::decode(&data.flatten()).unwrap()
    }
}

pub fn encode<T: Encode + Layout>(value: &T) -> lutra_bin::Data {
    let mut buf = bytes::BytesMut::with_capacity(T::head_size() / 8);
    value.encode(&mut buf);
    lutra_bin::Data::new(buf.to_vec())
}
