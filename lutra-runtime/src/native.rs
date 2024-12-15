use lutra_bin::ir;
use lutra_bin::{Encode, Layout};

use crate::interpreter::{Cell, Interpreter};
use crate::NativeModule;

pub mod std {
    use crate::native::*;
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
            pub fn $name(_: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
                let left = $left_assume(&args[0].1);
                let right = $right_assume(&args[1].1);

                let res = left $op right;
                Cell::Data(encode(&res))
            }
        };
    }

    macro_rules! un_func {
        ($name: ident, $assume: path, $op: tt) => {
            pub fn $name(_: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
                let operand = $assume(&args[0].1);
                let res = $op operand;
                Cell::Data(encode(&res))
            }
        };
    }

    macro_rules! reduce_func {
        ($name: ident, $item_decode: path, $reduce: expr, $default: literal) => {
            pub fn $name(_: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
                let array = assume::array(args[0].0, &args[0].1);

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

        pub fn index(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let mut input = assume::array(args[0].0, &args[0].1);

            let offset = assume::int(&args[1].1);

            let item = input.nth(offset as usize).unwrap();
            Cell::Data(item)
        }

        pub fn map(it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let input = assume::array(args[0].0, &args[0].1);
            let input_item_ty = args[0].0.kind.as_array().unwrap().as_ref();

            let func = &args[1];
            let func_ty = &func.0.kind.as_function().unwrap();

            let output_ty = construct_adhoc_ty(ir::TyKind::Array(Box::new(func_ty.body.clone())));

            let mut res = ArrayWriter::new(&output_ty);

            for item in input {
                let cell = Cell::Data(item);

                let value = it.evaluate_func_call(&func.1, vec![(input_item_ty, cell)]);

                res.write_item(assume::into_value(value));
            }

            Cell::Data(res.finish())
        }

        pub fn filter(it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let input = assume::array(args[0].0, &args[0].1);
            let input_item_ty = args[0].0.kind.as_array().unwrap().as_ref();

            let func = &args[1];

            let mut output = ArrayWriter::new(args[0].0);
            for item in input {
                let item_c = Cell::Data(item.clone());

                let condition = it.evaluate_func_call(&func.1, vec![(input_item_ty, item_c)]);
                let condition = assume::bool(&condition);

                if condition {
                    output.write_item(item);
                }
            }

            Cell::Data(output.finish())
        }

        pub fn slice(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let input = assume::array(args[0].0, &args[0].1);

            // unpack
            let start = assume::int(&args[1].1);
            let end = assume::int(&args[2].1);

            // convert to absolute
            let start = index_rel_to_abs(start, input.remaining());
            let end = index_rel_to_abs(end, input.remaining());

            // convert to skip & take
            let skip = start;
            let take = end.saturating_sub(skip);

            let mut output = ArrayWriter::new(args[0].0);
            for item in input.skip(start).take(take) {
                output.write_item(item);
            }
            Cell::Data(output.finish())
        }

        pub fn sort(it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let input = assume::array(args[0].0, &args[0].1);
            let input_item_ty = args[0].0.kind.as_array().unwrap().as_ref();

            let func = &args[1];

            let mut keys = Vec::with_capacity(input.remaining());
            for (index, item) in input.clone().enumerate() {
                let cell = Cell::Data(item);

                let key = it.evaluate_func_call(&func.1, vec![(input_item_ty, cell)]);
                let key = assume::int(&key);

                keys.push((key, index));
            }
            keys.sort();

            let mut output = ArrayWriter::new(args[0].0);
            for (_key, index) in keys {
                let item = input.get(index).unwrap();
                output.write_item(item);
            }
            Cell::Data(output.finish())
        }

        pub fn to_columnar(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let input = assume::array(args[0].0, &args[0].1);
            let input_items_ty = args[0].0.kind.as_array().unwrap().as_ref();
            let input_fields_ty = input_items_ty.kind.as_tuple().unwrap();

            let output_fields_ty: Vec<_> = input_fields_ty
                .iter()
                .map(|f| ir::TyTupleField {
                    name: f.name.clone(),
                    ty: construct_adhoc_ty(ir::TyKind::Array(Box::new(f.ty.clone()))),
                })
                .collect();
            let output_ty = construct_adhoc_ty(ir::TyKind::Tuple(output_fields_ty.clone()));

            // init output arrays
            let mut output_arrays: Vec<ArrayWriter> = output_fields_ty
                .iter()
                .map(|f| ArrayWriter::new(&f.ty))
                .collect();

            // partition input into output arrays
            for item in input {
                let tuple = TupleReader::new(&item, input_items_ty);

                for (index, output_array) in output_arrays.iter_mut().enumerate() {
                    let field = tuple.get_field(index);
                    output_array.write_item(field);
                }
            }

            // construct the final tuple
            let mut output = TupleWriter::new(&output_ty);
            for output_array in output_arrays {
                let array = output_array.finish();
                output.write_field(array);
            }
            Cell::Data(output.finish())
        }

        pub fn from_columnar(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let input = assume::as_value(&args[0].1);
            let input = TupleReader::new(input, args[0].0);

            let input_fields_ty = args[0].0.kind.as_tuple().unwrap();

            let output_tuple_ty = construct_adhoc_ty(ir::TyKind::Tuple(
                input_fields_ty
                    .iter()
                    .map(|f| {
                        let items_ty = f.ty.kind.as_array().unwrap().clone();
                        ir::TyTupleField {
                            name: f.name.clone(),
                            ty: *items_ty,
                        }
                    })
                    .collect(),
            ));
            let output_ty =
                construct_adhoc_ty(ir::TyKind::Array(Box::new(output_tuple_ty.clone())));

            // init input array readers
            let mut input_arrays: Vec<ArrayReader> = input_fields_ty
                .iter()
                .enumerate()
                .map(|(index, f)| ArrayReader::new_for_ty(input.get_field(index), &f.ty))
                .collect();

            // init output array
            let mut output = ArrayWriter::new(&output_ty);
            'output: loop {
                let mut tuple = TupleWriter::new(&output_tuple_ty);
                for (index, _) in input_fields_ty.iter().enumerate() {
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

        pub fn count(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let array = assume::array(args[0].0, &args[0].1);
            let res = array.count() as i64;
            Cell::Data(encode(&res))
        }

        pub fn average(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let array = assume::array(args[0].0, &args[0].1);

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

        pub fn all(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let mut array = assume::array(args[0].0, &args[0].1);

            let res = array.all(|x| decode::bool(&x));
            Cell::Data(encode(&res))
        }

        pub fn any(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let mut array = assume::array(args[0].0, &args[0].1);

            let res = array.any(|x| decode::bool(&x));
            Cell::Data(encode(&res))
        }

        pub fn contains(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let array = assume::array(args[0].0, &args[0].1);
            let item = assume::int(&args[1].1);

            let res = array.into_iter().any(|x| decode::int(&x) == item);
            Cell::Data(encode(&res))
        }

        pub fn concat_array(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let array = assume::array(args[0].0, &args[0].1);
            let separator = assume::text(&args[1].1);

            let array: Vec<_> = array.map(|x| decode::text(&x)).collect();
            let res = array.join(&separator);
            Cell::Data(encode(&res))
        }

        pub fn lag(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let array = assume::array(args[0].0, &args[0].1);
            let offset = assume::int(&args[1].1).max(0) as usize;

            let mut out = ArrayWriter::new(args[0].0);

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

        pub fn lead(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let array = assume::array(args[0].0, &args[0].1);
            let offset = assume::int(&args[1].1).max(0) as usize;

            let mut out = ArrayWriter::new(args[0].0);
            let array_len = array.remaining();

            for item in array.skip(offset) {
                out.write_item(item);
            }

            let n_blanks = offset.min(array_len);
            for _ in 0..n_blanks {
                out.write_item(encode(&0_i64));
            }

            Cell::Data(out.finish())
        }

        pub fn row_number(_it: &mut Interpreter, args: Vec<(&ir::Ty, Cell)>) -> Cell {
            let array = assume::array(args[0].0, &args[0].1);

            let mut out = ArrayWriter::new(args[0].0); // TODO: make this int always
            for (index, _) in array.enumerate() {
                out.write_item(encode(&(index as i64)));
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
        fn version(_it: &mut Interpreter, _args: Vec<(&ir::Ty, Cell)>) -> Cell {
            Cell::Data(encode(&"lutra-runtime 0.0.1".to_string()))
        }
    }
}

mod assume {
    use super::decode;
    use crate::interpreter::Cell;
    use lutra_bin::ir;
    use lutra_bin::ArrayReader;

    pub fn into_value(cell: Cell) -> lutra_bin::Data {
        match cell {
            Cell::Data(val) => val,
            Cell::Function(_) => panic!(),
            Cell::FunctionNative(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }

    pub fn as_value(cell: &Cell) -> &lutra_bin::Data {
        match cell {
            Cell::Data(val) => val,
            Cell::Function(_) => panic!(),
            Cell::FunctionNative(_) => panic!(),
            Cell::Vacant => panic!(),
        }
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

    pub fn array(ty: &ir::Ty, cell: &Cell) -> ArrayReader {
        let data = as_value(cell).clone();
        ArrayReader::new_for_ty(data, ty)
    }
}

mod decode {
    use lutra_bin::{Data, Decode};

    pub fn int(data: &Data) -> i64 {
        i64::decode_buffer(data.slice(8)).unwrap()
    }

    pub fn bool(data: &Data) -> bool {
        bool::decode_buffer(data.slice(1)).unwrap()
    }

    pub fn text(data: &Data) -> String {
        // TODO: string reader
        String::decode_buffer(&data.flatten()).unwrap()
    }
}

pub fn encode<T: Encode + Layout>(value: &T) -> lutra_bin::Data {
    let mut buf = Vec::with_capacity(T::head_size() / 8);
    value.encode(&mut buf).unwrap();
    lutra_bin::Data::new(buf)
}

fn construct_adhoc_ty(kind: ir::TyKind) -> ir::Ty {
    let mut ty = ir::Ty {
        kind,
        layout: None,
        name: None,
    };
    ty.layout = Some(lutra_bin::layout::get_layout_simple(&ty).unwrap());
    ty
}
