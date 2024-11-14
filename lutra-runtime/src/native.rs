use std::rc::Rc;

use lutra_bin::{Encode, Layout};

use crate::interpreter::{Cell, Interpreter};
use crate::NativeModule;

pub mod core {
    pub mod int {
        use crate::native::*;

        pub const MODULE: Module = Module;

        pub struct Module;

        impl NativeModule for Module {
            fn lookup_native_symbol(&self, id: &str) -> crate::interpreter::Cell {
                match id {
                    "add" => Cell::FunctionNative(&Self::add),
                    "sub" => Cell::FunctionNative(&Self::sub),
                    "mul" => Cell::FunctionNative(&Self::mul),
                    _ => panic!(),
                }
            }
        }

        impl Module {
            pub fn add(_: &mut Interpreter, _: Vec<u32>, args: Vec<Cell>) -> Cell {
                let left = assume::int(&args[0]);
                let right = assume::int(&args[1]);

                Cell::Value(encode(&(left + right)))
            }

            pub fn sub(_: &mut Interpreter, _: Vec<u32>, args: Vec<Cell>) -> Cell {
                let left = assume::int(&args[0]);
                let right = assume::int(&args[1]);

                Cell::Value(encode(&(left - right)))
            }

            pub fn mul(_: &mut Interpreter, _: Vec<u32>, args: Vec<Cell>) -> Cell {
                let left = assume::int(&args[0]);
                let right = assume::int(&args[1]);

                Cell::Value(encode(&(left * right)))
            }
        }
    }

    pub mod array {
        use crate::native::*;

        pub const MODULE: Module = Module;
        pub struct Module;

        impl NativeModule for Module {
            fn lookup_native_symbol(&self, id: &str) -> crate::interpreter::Cell {
                match id {
                    "map" => Cell::FunctionNative(&Self::map),
                    _ => panic!(),
                }
            }
        }

        impl Module {
            pub fn map(it: &mut Interpreter, layout: Vec<u32>, args: Vec<Cell>) -> Cell {
                let input_item_head_bytes = layout[0];

                let func = &args[0];
                let array = assume::array(&args[1], input_item_head_bytes as usize);

                let mut res = vec![8, 0, 0, 0];
                res.extend((array.remaining() as u32).to_le_bytes());

                for item in array {
                    // TODO: this is super inefficient
                    let cell = Cell::Value(item.to_owned().into());

                    let value = it.evaluate_func_call(func, vec![], vec![cell]);

                    // TODO: this does not work for types with body
                    res.extend(dbg!(assume::as_value(&value)));
                }

                Cell::Value(Rc::new(res))
            }
        }
    }
}

pub mod interpreter {
    use crate::native::*;

    pub const MODULE: Module = Module;
    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> crate::interpreter::Cell {
            match id {
                "version" => Self::version(),
                _ => panic!(),
            }
        }
    }

    impl Module {
        fn version() -> Cell {
            Cell::Value(encode(&"lutra-runtime 0.0.1".to_string()))
        }
    }
}

mod assume {
    use crate::interpreter::Cell;
    use lutra_bin::{ArrayReader, Decode, Reader};

    #[allow(dead_code)]
    pub fn into_value(cell: &Cell) -> std::rc::Rc<Vec<u8>> {
        match cell {
            Cell::Value(val) => val.clone(),
            Cell::Function(_) => panic!(),
            Cell::FunctionNative(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }

    pub fn as_value(cell: &Cell) -> &[u8] {
        match cell {
            Cell::Value(val) => val.as_ref(),
            Cell::Function(_) => panic!(),
            Cell::FunctionNative(_) => panic!(),
            Cell::Vacant => panic!(),
        }
    }

    pub fn int(cell: &Cell) -> i64 {
        let bytes = as_value(cell);
        i64::decode_buffer(bytes).unwrap()
    }

    pub fn array(cell: &Cell, item_head_bytes: usize) -> ArrayReader<'_> {
        let mut reader = Reader::new(as_value(cell));
        ArrayReader::new(&mut reader, item_head_bytes)
    }
}

pub fn encode<T: Encode + Layout>(value: &T) -> Rc<Vec<u8>> {
    let mut buf = Vec::with_capacity(T::head_size() / 8);
    value.encode(&mut buf).unwrap();
    Rc::new(buf)
}
