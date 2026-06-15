use lutra_bin::{Encode, Layout};

use crate::interpreter::{Cell, Interpreter};
use crate::{Data, NativeModule};

/// Generates a typed cast function: source type → target type, no layout args.
macro_rules! typed_cast {
    ($name: ident, $src: ty, $dst: ty) => {
        pub fn $name(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
            let [x] = assume::exactly_n(args);
            let v = assume::primitive::<$src>(&x)? as $dst;
            Ok(Cell::Data(encode(&v)))
        }
    };
}

/// Generates a typed binary op function that needs no layout args.
/// The Rust type is baked into the function.
macro_rules! typed_bin_op {
    ($name: ident, $prim: ty, $op: tt) => {
        pub fn $name(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
            let left = assume::primitive::<$prim>(&args[0])?;
            let right = assume::primitive::<$prim>(&args[1])?;
            Ok(encode_cell(&(left $op right)))
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

pub mod std_convert {
    use crate::EvalError;
    use crate::native::*;

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                // to_int8 casts
                "to_int8_int8" => &to_int8_int8,
                "to_int8_int16" => &to_int8_int16,
                "to_int8_int32" => &to_int8_int32,
                "to_int8_int64" => &to_int8_int64,
                "to_int8_uint8" => &to_int8_uint8,
                "to_int8_uint16" => &to_int8_uint16,
                "to_int8_uint32" => &to_int8_uint32,
                "to_int8_uint64" => &to_int8_uint64,
                "to_int8_float32" => &to_int8_float32,
                "to_int8_float64" => &to_int8_float64,
                // to_int16 casts
                "to_int16_int8" => &to_int16_int8,
                "to_int16_int16" => &to_int16_int16,
                "to_int16_int32" => &to_int16_int32,
                "to_int16_int64" => &to_int16_int64,
                "to_int16_uint8" => &to_int16_uint8,
                "to_int16_uint16" => &to_int16_uint16,
                "to_int16_uint32" => &to_int16_uint32,
                "to_int16_uint64" => &to_int16_uint64,
                "to_int16_float32" => &to_int16_float32,
                "to_int16_float64" => &to_int16_float64,
                // to_int32 casts
                "to_int32_int8" => &to_int32_int8,
                "to_int32_int16" => &to_int32_int16,
                "to_int32_int32" => &to_int32_int32,
                "to_int32_int64" => &to_int32_int64,
                "to_int32_uint8" => &to_int32_uint8,
                "to_int32_uint16" => &to_int32_uint16,
                "to_int32_uint32" => &to_int32_uint32,
                "to_int32_uint64" => &to_int32_uint64,
                "to_int32_float32" => &to_int32_float32,
                "to_int32_float64" => &to_int32_float64,
                // to_int64 casts
                "to_int64_int8" => &to_int64_int8,
                "to_int64_int16" => &to_int64_int16,
                "to_int64_int32" => &to_int64_int32,
                "to_int64_int64" => &to_int64_int64,
                "to_int64_uint8" => &to_int64_uint8,
                "to_int64_uint16" => &to_int64_uint16,
                "to_int64_uint32" => &to_int64_uint32,
                "to_int64_uint64" => &to_int64_uint64,
                "to_int64_float32" => &to_int64_float32,
                "to_int64_float64" => &to_int64_float64,
                // to_uint8 casts
                "to_uint8_int8" => &to_uint8_int8,
                "to_uint8_int16" => &to_uint8_int16,
                "to_uint8_int32" => &to_uint8_int32,
                "to_uint8_int64" => &to_uint8_int64,
                "to_uint8_uint8" => &to_uint8_uint8,
                "to_uint8_uint16" => &to_uint8_uint16,
                "to_uint8_uint32" => &to_uint8_uint32,
                "to_uint8_uint64" => &to_uint8_uint64,
                "to_uint8_float32" => &to_uint8_float32,
                "to_uint8_float64" => &to_uint8_float64,
                // to_uint16 casts
                "to_uint16_int8" => &to_uint16_int8,
                "to_uint16_int16" => &to_uint16_int16,
                "to_uint16_int32" => &to_uint16_int32,
                "to_uint16_int64" => &to_uint16_int64,
                "to_uint16_uint8" => &to_uint16_uint8,
                "to_uint16_uint16" => &to_uint16_uint16,
                "to_uint16_uint32" => &to_uint16_uint32,
                "to_uint16_uint64" => &to_uint16_uint64,
                "to_uint16_float32" => &to_uint16_float32,
                "to_uint16_float64" => &to_uint16_float64,
                // to_uint32 casts
                "to_uint32_int8" => &to_uint32_int8,
                "to_uint32_int16" => &to_uint32_int16,
                "to_uint32_int32" => &to_uint32_int32,
                "to_uint32_int64" => &to_uint32_int64,
                "to_uint32_uint8" => &to_uint32_uint8,
                "to_uint32_uint16" => &to_uint32_uint16,
                "to_uint32_uint32" => &to_uint32_uint32,
                "to_uint32_uint64" => &to_uint32_uint64,
                "to_uint32_float32" => &to_uint32_float32,
                "to_uint32_float64" => &to_uint32_float64,
                // to_uint64 casts
                "to_uint64_int8" => &to_uint64_int8,
                "to_uint64_int16" => &to_uint64_int16,
                "to_uint64_int32" => &to_uint64_int32,
                "to_uint64_int64" => &to_uint64_int64,
                "to_uint64_uint8" => &to_uint64_uint8,
                "to_uint64_uint16" => &to_uint64_uint16,
                "to_uint64_uint32" => &to_uint64_uint32,
                "to_uint64_uint64" => &to_uint64_uint64,
                "to_uint64_float32" => &to_uint64_float32,
                "to_uint64_float64" => &to_uint64_float64,
                // to_float32 casts
                "to_float32_int8" => &to_float32_int8,
                "to_float32_int16" => &to_float32_int16,
                "to_float32_int32" => &to_float32_int32,
                "to_float32_int64" => &to_float32_int64,
                "to_float32_uint8" => &to_float32_uint8,
                "to_float32_uint16" => &to_float32_uint16,
                "to_float32_uint32" => &to_float32_uint32,
                "to_float32_uint64" => &to_float32_uint64,
                "to_float32_float32" => &to_float32_float32,
                "to_float32_float64" => &to_float32_float64,
                // to_float64 casts
                "to_float64_int8" => &to_float64_int8,
                "to_float64_int16" => &to_float64_int16,
                "to_float64_int32" => &to_float64_int32,
                "to_float64_int64" => &to_float64_int64,
                "to_float64_uint8" => &to_float64_uint8,
                "to_float64_uint16" => &to_float64_uint16,
                "to_float64_uint32" => &to_float64_uint32,
                "to_float64_uint64" => &to_float64_uint64,
                "to_float64_float32" => &to_float64_float32,
                "to_float64_float64" => &to_float64_float64,
                // to_text casts
                "to_text_bool" => &to_text_bool,
                "to_text_int8" => &to_text_int8,
                "to_text_int16" => &to_text_int16,
                "to_text_int32" => &to_text_int32,
                "to_text_int64" => &to_text_int64,
                "to_text_uint8" => &to_text_uint8,
                "to_text_uint16" => &to_text_uint16,
                "to_text_uint32" => &to_text_uint32,
                "to_text_uint64" => &to_text_uint64,
                "to_text_float32" => &to_text_float32,
                "to_text_float64" => &to_text_float64,
                "to_text_text" => &to_text_text,
                _ => return None,
            })
        }
    }

    // Numeric casts: 10 target types × 10 source types = 100 functions
    typed_cast!(to_int8_int8, i8, i8);
    typed_cast!(to_int8_int16, i16, i8);
    typed_cast!(to_int8_int32, i32, i8);
    typed_cast!(to_int8_int64, i64, i8);
    typed_cast!(to_int8_uint8, u8, i8);
    typed_cast!(to_int8_uint16, u16, i8);
    typed_cast!(to_int8_uint32, u32, i8);
    typed_cast!(to_int8_uint64, u64, i8);
    typed_cast!(to_int8_float32, f32, i8);
    typed_cast!(to_int8_float64, f64, i8);

    typed_cast!(to_int16_int8, i8, i16);
    typed_cast!(to_int16_int16, i16, i16);
    typed_cast!(to_int16_int32, i32, i16);
    typed_cast!(to_int16_int64, i64, i16);
    typed_cast!(to_int16_uint8, u8, i16);
    typed_cast!(to_int16_uint16, u16, i16);
    typed_cast!(to_int16_uint32, u32, i16);
    typed_cast!(to_int16_uint64, u64, i16);
    typed_cast!(to_int16_float32, f32, i16);
    typed_cast!(to_int16_float64, f64, i16);

    typed_cast!(to_int32_int8, i8, i32);
    typed_cast!(to_int32_int16, i16, i32);
    typed_cast!(to_int32_int32, i32, i32);
    typed_cast!(to_int32_int64, i64, i32);
    typed_cast!(to_int32_uint8, u8, i32);
    typed_cast!(to_int32_uint16, u16, i32);
    typed_cast!(to_int32_uint32, u32, i32);
    typed_cast!(to_int32_uint64, u64, i32);
    typed_cast!(to_int32_float32, f32, i32);
    typed_cast!(to_int32_float64, f64, i32);

    typed_cast!(to_int64_int8, i8, i64);
    typed_cast!(to_int64_int16, i16, i64);
    typed_cast!(to_int64_int32, i32, i64);
    typed_cast!(to_int64_int64, i64, i64);
    typed_cast!(to_int64_uint8, u8, i64);
    typed_cast!(to_int64_uint16, u16, i64);
    typed_cast!(to_int64_uint32, u32, i64);
    typed_cast!(to_int64_uint64, u64, i64);
    typed_cast!(to_int64_float32, f32, i64);
    typed_cast!(to_int64_float64, f64, i64);

    typed_cast!(to_uint8_int8, i8, u8);
    typed_cast!(to_uint8_int16, i16, u8);
    typed_cast!(to_uint8_int32, i32, u8);
    typed_cast!(to_uint8_int64, i64, u8);
    typed_cast!(to_uint8_uint8, u8, u8);
    typed_cast!(to_uint8_uint16, u16, u8);
    typed_cast!(to_uint8_uint32, u32, u8);
    typed_cast!(to_uint8_uint64, u64, u8);
    typed_cast!(to_uint8_float32, f32, u8);
    typed_cast!(to_uint8_float64, f64, u8);

    typed_cast!(to_uint16_int8, i8, u16);
    typed_cast!(to_uint16_int16, i16, u16);
    typed_cast!(to_uint16_int32, i32, u16);
    typed_cast!(to_uint16_int64, i64, u16);
    typed_cast!(to_uint16_uint8, u8, u16);
    typed_cast!(to_uint16_uint16, u16, u16);
    typed_cast!(to_uint16_uint32, u32, u16);
    typed_cast!(to_uint16_uint64, u64, u16);
    typed_cast!(to_uint16_float32, f32, u16);
    typed_cast!(to_uint16_float64, f64, u16);

    typed_cast!(to_uint32_int8, i8, u32);
    typed_cast!(to_uint32_int16, i16, u32);
    typed_cast!(to_uint32_int32, i32, u32);
    typed_cast!(to_uint32_int64, i64, u32);
    typed_cast!(to_uint32_uint8, u8, u32);
    typed_cast!(to_uint32_uint16, u16, u32);
    typed_cast!(to_uint32_uint32, u32, u32);
    typed_cast!(to_uint32_uint64, u64, u32);
    typed_cast!(to_uint32_float32, f32, u32);
    typed_cast!(to_uint32_float64, f64, u32);

    typed_cast!(to_uint64_int8, i8, u64);
    typed_cast!(to_uint64_int16, i16, u64);
    typed_cast!(to_uint64_int32, i32, u64);
    typed_cast!(to_uint64_int64, i64, u64);
    typed_cast!(to_uint64_uint8, u8, u64);
    typed_cast!(to_uint64_uint16, u16, u64);
    typed_cast!(to_uint64_uint32, u32, u64);
    typed_cast!(to_uint64_uint64, u64, u64);
    typed_cast!(to_uint64_float32, f32, u64);
    typed_cast!(to_uint64_float64, f64, u64);

    typed_cast!(to_float32_int8, i8, f32);
    typed_cast!(to_float32_int16, i16, f32);
    typed_cast!(to_float32_int32, i32, f32);
    typed_cast!(to_float32_int64, i64, f32);
    typed_cast!(to_float32_uint8, u8, f32);
    typed_cast!(to_float32_uint16, u16, f32);
    typed_cast!(to_float32_uint32, u32, f32);
    typed_cast!(to_float32_uint64, u64, f32);
    typed_cast!(to_float32_float32, f32, f32);
    typed_cast!(to_float32_float64, f64, f32);

    typed_cast!(to_float64_int8, i8, f64);
    typed_cast!(to_float64_int16, i16, f64);
    typed_cast!(to_float64_int32, i32, f64);
    typed_cast!(to_float64_int64, i64, f64);
    typed_cast!(to_float64_uint8, u8, f64);
    typed_cast!(to_float64_uint16, u16, f64);
    typed_cast!(to_float64_uint32, u32, f64);
    typed_cast!(to_float64_uint64, u64, f64);
    typed_cast!(to_float64_float32, f32, f64);
    typed_cast!(to_float64_float64, f64, f64);

    // to_text casts: per source type
    macro_rules! typed_to_text {
        ($name: ident, $src: ty) => {
            pub fn $name(
                _: &mut Interpreter,
                _: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let [x] = assume::exactly_n(args);
                let s = assume::primitive::<$src>(&x)?.to_string();
                Ok(Cell::Data(encode(&s)))
            }
        };
    }
    typed_to_text!(to_text_int8, i8);
    typed_to_text!(to_text_int16, i16);
    typed_to_text!(to_text_int32, i32);
    typed_to_text!(to_text_int64, i64);
    typed_to_text!(to_text_uint8, u8);
    typed_to_text!(to_text_uint16, u16);
    typed_to_text!(to_text_uint32, u32);
    typed_to_text!(to_text_uint64, u64);
    typed_to_text!(to_text_float32, f32);
    typed_to_text!(to_text_float64, f64);
    typed_to_text!(to_text_bool, bool);
    pub fn to_text_text(
        _: &mut Interpreter,
        _: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [x] = assume::exactly_n(args);
        Ok(x)
    }
}

pub mod std_ops {
    use ::std::cmp::Ordering;

    use crate::native::assume::exactly_n;
    use crate::native::*;
    use crate::{Data, EvalError};

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "div_int8" => &div_int8,
                "div_int16" => &div_int16,
                "div_int32" => &div_int32,
                "div_int64" => &div_int64,
                "div_uint8" => &div_uint8,
                "div_uint16" => &div_uint16,
                "div_uint32" => &div_uint32,
                "div_uint64" => &div_uint64,
                "div_float32" => &div_float32,
                "div_float64" => &div_float64,

                "mod_int8" => &mod_int8,
                "mod_int16" => &mod_int16,
                "mod_int32" => &mod_int32,
                "mod_int64" => &mod_int64,
                "mod_uint8" => &mod_uint8,
                "mod_uint16" => &mod_uint16,
                "mod_uint32" => &mod_uint32,
                "mod_uint64" => &mod_uint64,
                "mod_float32" => &mod_float32,
                "mod_float64" => &mod_float64,

                "neg_int8" => &neg_int8,
                "neg_int16" => &neg_int16,
                "neg_int32" => &neg_int32,
                "neg_int64" => &neg_int64,
                "neg_float32" => &neg_float32,
                "neg_float64" => &neg_float64,
                "neg_duration" => &neg_int64,

                "add_int8" => &add_int8,
                "add_int16" => &add_int16,
                "add_int32" => &add_int32,
                "add_int64" => &add_int64,
                "add_uint8" => &add_uint8,
                "add_uint16" => &add_uint16,
                "add_uint32" => &add_uint32,
                "add_uint64" => &add_uint64,
                "add_float32" => &add_float32,
                "add_float64" => &add_float64,
                "add_duration" => &add_int64,
                "add_decimal" => &add_int64,

                "sub_int8" => &sub_int8,
                "sub_int16" => &sub_int16,
                "sub_int32" => &sub_int32,
                "sub_int64" => &sub_int64,
                "sub_uint8" => &sub_uint8,
                "sub_uint16" => &sub_uint16,
                "sub_uint32" => &sub_uint32,
                "sub_uint64" => &sub_uint64,
                "sub_float32" => &sub_float32,
                "sub_float64" => &sub_float64,
                "sub_duration" => &sub_int64,
                "sub_decimal" => &sub_int64,

                "mul_int8" => &mul_int8,
                "mul_int16" => &mul_int16,
                "mul_int32" => &mul_int32,
                "mul_int64" => &mul_int64,
                "mul_uint8" => &mul_uint8,
                "mul_uint16" => &mul_uint16,
                "mul_uint32" => &mul_uint32,
                "mul_uint64" => &mul_uint64,
                "mul_float32" => &mul_float32,
                "mul_float64" => &mul_float64,

                "cmp_bool" => &cmp_bool,
                "cmp_int8" => &cmp_int8,
                "cmp_int16" => &cmp_int16,
                "cmp_int32" => &cmp_int32,
                "cmp_int64" => &cmp_int64,
                "cmp_uint8" => &cmp_uint8,
                "cmp_uint16" => &cmp_uint16,
                "cmp_uint32" => &cmp_uint32,
                "cmp_uint64" => &cmp_uint64,
                "cmp_float32" => &cmp_float32,
                "cmp_float64" => &cmp_float64,
                "cmp_text" => &cmp_text,
                "cmp_date" => &cmp_int32,
                "cmp_time" => &cmp_int64,
                "cmp_duration" => &cmp_int64,
                "cmp_timestamp" => &cmp_int64,
                "cmp_decimal" => &cmp_int64,

                "eq_bool" => &eq_bool,
                "eq_int8" => &eq_int8,
                "eq_int16" => &eq_int16,
                "eq_int32" => &eq_int32,
                "eq_int64" => &eq_int64,
                "eq_uint8" => &eq_uint8,
                "eq_uint16" => &eq_uint16,
                "eq_uint32" => &eq_uint32,
                "eq_uint64" => &eq_uint64,
                "eq_float32" => &eq_float32,
                "eq_float64" => &eq_float64,
                "eq_text" => &eq_text,
                "eq_date" => &eq_int32,
                "eq_time" => &eq_int64,
                "eq_duration" => &eq_int64,
                "eq_timestamp" => &eq_int64,
                "eq_decimal" => &eq_int64,

                "lt_bool" => &lt_bool,
                "lt_int8" => &lt_int8,
                "lt_int16" => &lt_int16,
                "lt_int32" => &lt_int32,
                "lt_int64" => &lt_int64,
                "lt_uint8" => &lt_uint8,
                "lt_uint16" => &lt_uint16,
                "lt_uint32" => &lt_uint32,
                "lt_uint64" => &lt_uint64,
                "lt_float32" => &lt_float32,
                "lt_float64" => &lt_float64,
                "lt_text" => &lt_text,
                "lt_date" => &lt_int32,
                "lt_time" => &lt_int64,
                "lt_timestamp" => &lt_int64,
                "lt_decimal" => &lt_int64,

                "lte_bool" => &lte_bool,
                "lte_int8" => &lte_int8,
                "lte_int16" => &lte_int16,
                "lte_int32" => &lte_int32,
                "lte_int64" => &lte_int64,
                "lte_uint8" => &lte_uint8,
                "lte_uint16" => &lte_uint16,
                "lte_uint32" => &lte_uint32,
                "lte_uint64" => &lte_uint64,
                "lte_float32" => &lte_float32,
                "lte_float64" => &lte_float64,
                "lte_text" => &lte_text,
                "lte_date" => &lte_int32,
                "lte_time" => &lte_int64,
                "lte_timestamp" => &lte_int64,
                "lte_decimal" => &lte_int64,

                "and" => &and,
                "or" => &or,
                "not" => &not,

                _ => return None,
            })
        }
    }

    // Per-type add functions (no layout args needed)
    typed_bin_op!(add_int8, i8, +);
    typed_bin_op!(add_int16, i16, +);
    typed_bin_op!(add_int32, i32, +);
    typed_bin_op!(add_int64, i64, +);
    typed_bin_op!(add_uint8, u8, +);
    typed_bin_op!(add_uint16, u16, +);
    typed_bin_op!(add_uint32, u32, +);
    typed_bin_op!(add_uint64, u64, +);
    typed_bin_op!(add_float32, f32, +);
    typed_bin_op!(add_float64, f64, +);

    // Per-type sub functions
    typed_bin_op!(sub_int8, i8, -);
    typed_bin_op!(sub_int16, i16, -);
    typed_bin_op!(sub_int32, i32, -);
    typed_bin_op!(sub_int64, i64, -);
    typed_bin_op!(sub_uint8, u8, -);
    typed_bin_op!(sub_uint16, u16, -);
    typed_bin_op!(sub_uint32, u32, -);
    typed_bin_op!(sub_uint64, u64, -);
    typed_bin_op!(sub_float32, f32, -);
    typed_bin_op!(sub_float64, f64, -);

    // Per-type mul functions
    typed_bin_op!(mul_int8, i8, *);
    typed_bin_op!(mul_int16, i16, *);
    typed_bin_op!(mul_int32, i32, *);
    typed_bin_op!(mul_int64, i64, *);
    typed_bin_op!(mul_uint8, u8, *);
    typed_bin_op!(mul_uint16, u16, *);
    typed_bin_op!(mul_uint32, u32, *);
    typed_bin_op!(mul_uint64, u64, *);
    typed_bin_op!(mul_float32, f32, *);
    typed_bin_op!(mul_float64, f64, *);
    // Per-type div functions
    typed_bin_op!(div_int8, i8, /);
    typed_bin_op!(div_int16, i16, /);
    typed_bin_op!(div_int32, i32, /);
    typed_bin_op!(div_int64, i64, /);
    typed_bin_op!(div_uint8, u8, /);
    typed_bin_op!(div_uint16, u16, /);
    typed_bin_op!(div_uint32, u32, /);
    typed_bin_op!(div_uint64, u64, /);
    typed_bin_op!(div_float32, f32, /);
    typed_bin_op!(div_float64, f64, /);

    // Per-type mod functions
    typed_bin_op!(mod_int8, i8, %);
    typed_bin_op!(mod_int16, i16, %);
    typed_bin_op!(mod_int32, i32, %);
    typed_bin_op!(mod_int64, i64, %);
    typed_bin_op!(mod_uint8, u8, %);
    typed_bin_op!(mod_uint16, u16, %);
    typed_bin_op!(mod_uint32, u32, %);
    typed_bin_op!(mod_uint64, u64, %);
    typed_bin_op!(mod_float32, f32, %);
    typed_bin_op!(mod_float64, f64, %);

    // Per-type neg functions
    macro_rules! typed_neg {
        ($name: ident, $prim: ty) => {
            pub fn $name(
                _: &mut Interpreter,
                _: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                Ok(neg_arg!($prim, args))
            }
        };
    }
    typed_neg!(neg_int8, i8);
    typed_neg!(neg_int16, i16);
    typed_neg!(neg_int32, i32);
    typed_neg!(neg_int64, i64);
    typed_neg!(neg_float32, f32);
    typed_neg!(neg_float64, f64);

    // Per-type cmp functions
    macro_rules! typed_cmp {
        ($name: ident, $prim: ty, $method: ident) => {
            pub fn $name(
                _: &mut Interpreter,
                _: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let [a, b] = exactly_n(args);
                let a = assume::into_data(a)?;
                let b = assume::into_data(b)?;
                let left = decode::primitive::<$prim>(&a);
                let right = decode::primitive::<$prim>(&b);
                let tag: u8 = match left.$method(&right) {
                    Ordering::Less => 0,
                    Ordering::Equal => 1,
                    Ordering::Greater => 2,
                };
                Ok(encode_cell(&tag))
            }
        };
    }
    typed_cmp!(cmp_bool, bool, cmp);
    typed_cmp!(cmp_int8, i8, cmp);
    typed_cmp!(cmp_int16, i16, cmp);
    typed_cmp!(cmp_int32, i32, cmp);
    typed_cmp!(cmp_int64, i64, cmp);
    typed_cmp!(cmp_uint8, u8, cmp);
    typed_cmp!(cmp_uint16, u16, cmp);
    typed_cmp!(cmp_uint32, u32, cmp);
    typed_cmp!(cmp_uint64, u64, cmp);
    typed_cmp!(cmp_float32, f32, total_cmp);
    typed_cmp!(cmp_float64, f64, total_cmp);
    pub fn cmp_text(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [a, b] = exactly_n(args);
        let a = decode::text_ref(assume::into_data(a)?);
        let b = decode::text_ref(assume::into_data(b)?);
        let tag: u8 = match a.as_str().cmp(b.as_str()) {
            Ordering::Less => 0,
            Ordering::Equal => 1,
            Ordering::Greater => 2,
        };
        Ok(encode_cell(&tag))
    }

    // Per-type eq functions
    macro_rules! typed_eq {
        ($name: ident, $prim: ty) => {
            pub fn $name(
                _: &mut Interpreter,
                _: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let left = assume::primitive::<$prim>(&args[0])?;
                let right = assume::primitive::<$prim>(&args[1])?;
                Ok(encode_cell(&left.eq(&right)))
            }
        };
    }
    typed_eq!(eq_bool, bool);
    typed_eq!(eq_int8, i8);
    typed_eq!(eq_int16, i16);
    typed_eq!(eq_int32, i32);
    typed_eq!(eq_int64, i64);
    typed_eq!(eq_uint8, u8);
    typed_eq!(eq_uint16, u16);
    typed_eq!(eq_uint32, u32);
    typed_eq!(eq_uint64, u64);
    typed_eq!(eq_float32, f32);
    typed_eq!(eq_float64, f64);
    pub fn eq_text(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [a, b] = exactly_n(args);
        let a = decode::text_ref(assume::into_data(a)?);
        let b = decode::text_ref(assume::into_data(b)?);
        Ok(encode_cell(&a.as_str().eq(b.as_str())))
    }

    // Per-type lt functions
    macro_rules! typed_lt {
        ($name: ident, $prim: ty) => {
            pub fn $name(
                _: &mut Interpreter,
                _: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let left = assume::primitive::<$prim>(&args[0])?;
                let right = assume::primitive::<$prim>(&args[1])?;
                Ok(encode_cell(&left.lt(&right)))
            }
        };
    }
    typed_lt!(lt_bool, bool);
    typed_lt!(lt_int8, i8);
    typed_lt!(lt_int16, i16);
    typed_lt!(lt_int32, i32);
    typed_lt!(lt_int64, i64);
    typed_lt!(lt_uint8, u8);
    typed_lt!(lt_uint16, u16);
    typed_lt!(lt_uint32, u32);
    typed_lt!(lt_uint64, u64);
    typed_lt!(lt_float32, f32);
    typed_lt!(lt_float64, f64);
    pub fn lt_text(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [a, b] = exactly_n(args);
        let a = decode::text_ref(assume::into_data(a)?);
        let b = decode::text_ref(assume::into_data(b)?);
        Ok(encode_cell(&a.as_str().lt(b.as_str())))
    }

    // Per-type lte functions
    macro_rules! typed_lte {
        ($name: ident, $prim: ty) => {
            pub fn $name(
                _: &mut Interpreter,
                _: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let left = assume::primitive::<$prim>(&args[0])?;
                let right = assume::primitive::<$prim>(&args[1])?;
                Ok(encode_cell(&left.le(&right)))
            }
        };
    }
    typed_lte!(lte_bool, bool);
    typed_lte!(lte_int8, i8);
    typed_lte!(lte_int16, i16);
    typed_lte!(lte_int32, i32);
    typed_lte!(lte_int64, i64);
    typed_lte!(lte_uint8, u8);
    typed_lte!(lte_uint16, u16);
    typed_lte!(lte_uint32, u32);
    typed_lte!(lte_uint64, u64);
    typed_lte!(lte_float32, f32);
    typed_lte!(lte_float64, f64);
    pub fn lte_text(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [a, b] = exactly_n(args);
        let a = decode::text_ref(assume::into_data(a)?);
        let b = decode::text_ref(assume::into_data(b)?);
        Ok(encode_cell(&a.as_str().le(b.as_str())))
    }

    pub fn eval_cmp(
        it: &mut Interpreter,
        cmp: &Cell,
        a: &Data,
        b: &Data,
    ) -> Result<Ordering, EvalError> {
        let ord = it.evaluate_func_call(cmp, vec![Cell::Data(a.clone()), Cell::Data(b.clone())])?;
        let tag = assume::primitive::<u8>(&ord)?;
        Ok(match tag {
            0 => Ordering::Less,
            1 => Ordering::Equal,
            2 => Ordering::Greater,
            _ => return Err(EvalError::BadProgram),
        })
    }

    pub fn and(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let a = assume::bool(&args[0])?;
        let b = assume::bool(&args[1])?;
        Ok(encode_cell(&(a && b)))
    }

    pub fn or(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let a = assume::bool(&args[0])?;
        let b = assume::bool(&args[1])?;
        Ok(encode_cell(&(a || b)))
    }

    pub fn not(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let operand = assume::bool(&args[0])?;
        Ok(encode_cell(&!operand))
    }
}

pub mod std_array {
    use ::std::borrow::Cow;
    use ::std::collections::HashMap;

    use crate::native::assume::LayoutArgsReader;
    use crate::native::*;
    use crate::{ArrayWriter, Data, EnumWriter, EvalError, TupleWriter};
    use lutra_bin::{ArrayReader, Decode, TupleReader};

    use ::std::cmp::Ordering;

    use super::std_ops::eval_cmp;

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
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
                "loop_until_empty" => &loop_until_empty,
                "sequence_int8" => &sequence_int8,
                "sequence_int16" => &sequence_int16,
                "sequence_int32" => &sequence_int32,
                "sequence_int64" => &sequence_int64,
                "sequence_uint8" => &sequence_uint8,
                "sequence_uint16" => &sequence_uint16,
                "sequence_uint32" => &sequence_uint32,
                "sequence_uint64" => &sequence_uint64,

                "min" => &min,
                "max" => &max,
                "sum_int8" => &sum_int8,
                "sum_int16" => &sum_int16,
                "sum_int32" => &sum_int32,
                "sum_int64" => &sum_int64,
                "sum_uint8" => &sum_uint8,
                "sum_uint16" => &sum_uint16,
                "sum_uint32" => &sum_uint32,
                "sum_uint64" => &sum_uint64,
                "sum_float32" => &sum_float32,
                "sum_float64" => &sum_float64,
                "mean_int8" => &mean_int8,
                "mean_int16" => &mean_int16,
                "mean_int32" => &mean_int32,
                "mean_int64" => &mean_int64,
                "mean_uint8" => &mean_uint8,
                "mean_uint16" => &mean_uint16,
                "mean_uint32" => &mean_uint32,
                "mean_uint64" => &mean_uint64,
                "mean_float32" => &mean_float32,
                "mean_float64" => &mean_float64,
                "all" => &all,
                "any" => &any,
                "count" => &count,

                "lag" => &lag,
                "lead" => &lead,
                "rolling_mean_int8" => &rolling_mean_int8,
                "rolling_mean_int16" => &rolling_mean_int16,
                "rolling_mean_int32" => &rolling_mean_int32,
                "rolling_mean_int64" => &rolling_mean_int64,
                "rolling_mean_uint8" => &rolling_mean_uint8,
                "rolling_mean_uint16" => &rolling_mean_uint16,
                "rolling_mean_uint32" => &rolling_mean_uint32,
                "rolling_mean_uint64" => &rolling_mean_uint64,
                "rolling_mean_float32" => &rolling_mean_float32,
                "rolling_mean_float64" => &rolling_mean_float64,
                "rank" => &rank,
                "rank_dense" => &rank_dense,
                "rank_percentile" => &rank_percentile,
                "cume_dist" => &cume_dist,

                _ => return None,
            })
        }
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

        let [array, func, cmp_func] = assume::exactly_n(args);

        let input = assume::array(array, item_head_bytes);

        let mut keys = Vec::with_capacity(input.remaining());
        for (index, item) in input.clone().enumerate() {
            let cell = Cell::Data(item);
            let key = it.evaluate_func_call(&func, vec![cell])?;
            keys.push((assume::into_data(key)?, index));
        }

        // Sort using injected comparator. We need try_sort_by but it's not stable,
        // so collect orderings eagerly via a fallible sort wrapper.
        let mut sort_err: Option<EvalError> = None;
        keys.sort_by(|(a, _), (b, _)| {
            if sort_err.is_some() {
                return Ordering::Equal;
            }
            match eval_cmp(it, &cmp_func, a, b) {
                Ok(ord) => ord,
                Err(e) => {
                    sort_err = Some(e);
                    Ordering::Equal
                }
            }
        });
        if let Some(e) = sort_err {
            return Err(e);
        }

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
        let key_head_bytes = layout_args.next_u32() as usize;

        let [input, key_getter] = assume::exactly_n(args);
        let input = ArrayReader::new(assume::into_data(input)?, input_head_bytes as usize);

        let mut groups: HashMap<Vec<u8>, Vec<Data>> = HashMap::new();
        for item in input {
            let item_cell = Cell::Data(item.clone());

            let key = it.evaluate_func_call(&key_getter, vec![item_cell])?;
            let key = assume::into_data(key)?.chunk()[..key_head_bytes].to_vec();

            let partition = groups.entry(key).or_default();
            partition.push(item);
        }

        // init output array
        let mut output = ArrayWriter::new(output_head_bytes, output_body_ptrs);
        for (key, values) in groups {
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

    pub fn loop_until_empty(
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

    macro_rules! sequence {
        ($ty: ident, $start: expr, $end: expr) => {{
            let start: $ty = decode::primitive(&$start);
            let end: $ty = decode::primitive(&$end);
            encode_primitives(start..end)
        }};
    }

    macro_rules! typed_sequence {
        ($name: ident, $prim: ident) => {
            pub fn $name(
                _: &mut Interpreter,
                _: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let [start, end] = assume::exactly_n(args);
                let start = assume::into_data(start)?;
                let end = assume::into_data(end)?;
                Ok(Cell::Data(sequence!($prim, start, end)))
            }
        };
    }
    typed_sequence!(sequence_int8, i8);
    typed_sequence!(sequence_int16, i16);
    typed_sequence!(sequence_int32, i32);
    typed_sequence!(sequence_int64, i64);
    typed_sequence!(sequence_uint8, u8);
    typed_sequence!(sequence_uint16, u16);
    typed_sequence!(sequence_uint32, u32);
    typed_sequence!(sequence_uint64, u64);

    pub fn min(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array, cmp_func] = assume::exactly_n(args);
        let array = assume::array(array, layout_args[0]);

        let mut res: Option<Data> = None;
        for item in array {
            res = Some(match res {
                None => item,
                Some(current) => {
                    let ord = eval_cmp(it, &cmp_func, &item, &current)?;
                    if ord.is_gt() { current } else { item }
                }
            });
        }
        Ok(Cell::Data(encode_option(res, layout_args[0])))
    }

    pub fn max(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array, cmp_func] = assume::exactly_n(args);
        let array = assume::array(array, layout_args[0]);

        let mut res: Option<Data> = None;
        for item in array {
            res = Some(match res {
                None => item,
                Some(current) => {
                    let ord = eval_cmp(it, &cmp_func, &item, &current)?;
                    if ord.is_lt() { current } else { item }
                }
            });
        }

        Ok(Cell::Data(encode_option(res, layout_args[0])))
    }

    macro_rules! typed_sum {
        ($name: ident, $prim: ty) => {
            pub fn $name(
                _: &mut Interpreter,
                layout_args: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let [array] = assume::exactly_n(args);
                let array = assume::array(array, layout_args[0]);
                let s = array
                    .map(|x| decode::primitive::<$prim>(&x))
                    .reduce(|a, b| a + b)
                    .unwrap_or_default();
                Ok(Cell::Data(encode(&s)))
            }
        };
    }
    typed_sum!(sum_int8, i8);
    typed_sum!(sum_int16, i16);
    typed_sum!(sum_int32, i32);
    typed_sum!(sum_int64, i64);
    typed_sum!(sum_uint8, u8);
    typed_sum!(sum_uint16, u16);
    typed_sum!(sum_uint32, u32);
    typed_sum!(sum_uint64, u64);
    typed_sum!(sum_float32, f32);
    typed_sum!(sum_float64, f64);

    macro_rules! typed_mean {
        ($name: ident, $prim: ty) => {
            pub fn $name(
                _: &mut Interpreter,
                layout_args: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let [array] = assume::exactly_n(args);
                let array = assume::array(array, layout_args[0]);
                let (sum, count) = array
                    .map(|x| decode::primitive::<$prim>(&x))
                    .fold((<$prim>::default(), 0_u32), |(sum, count), item| {
                        (sum + item, count + 1)
                    });
                Ok(Cell::Data(encode(&(sum as f64 / count as f64))))
            }
        };
    }
    typed_mean!(mean_int8, i8);
    typed_mean!(mean_int16, i16);
    typed_mean!(mean_int32, i32);
    typed_mean!(mean_int64, i64);
    typed_mean!(mean_uint8, u8);
    typed_mean!(mean_uint16, u16);
    typed_mean!(mean_uint32, u32);
    typed_mean!(mean_uint64, u64);
    typed_mean!(mean_float32, f32);
    typed_mean!(mean_float64, f64);

    pub fn count(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [array] = assume::exactly_n(args);
        let array = assume::into_data(array)?;

        let (_offset, len) = lutra_bin::ArrayReader::<&[u8]>::read_head(array.chunk());

        let res = len as i64;
        Ok(Cell::Data(encode(&res)))
    }

    pub fn all(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [array] = assume::exactly_n(args);
        let mut array = assume::array(array, 1);

        let res = array.all(|x| decode::bool(&x));
        Ok(Cell::Data(encode(&res)))
    }

    pub fn any(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [array] = assume::exactly_n(args);
        let mut array = assume::array(array, 1);

        let res = array.any(|x| decode::bool(&x));
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

    macro_rules! typed_rolling_mean {
        ($name: ident, $prim: ty) => {
            pub fn $name(
                _it: &mut Interpreter,
                layout_args: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let [array, trailing, leading] = assume::exactly_n(args);
                let trailing: u32 = assume::primitive(&trailing)?;
                let leading: u32 = assume::primitive(&leading)?;
                let array = assume::array(array, layout_args[0]);
                rolling_mean_impl::<$prim>(array, trailing, leading)
            }
        };
    }
    typed_rolling_mean!(rolling_mean_int8, i8);
    typed_rolling_mean!(rolling_mean_int16, i16);
    typed_rolling_mean!(rolling_mean_int32, i32);
    typed_rolling_mean!(rolling_mean_int64, i64);
    typed_rolling_mean!(rolling_mean_uint8, u8);
    typed_rolling_mean!(rolling_mean_uint16, u16);
    typed_rolling_mean!(rolling_mean_uint32, u32);
    typed_rolling_mean!(rolling_mean_uint64, u64);
    typed_rolling_mean!(rolling_mean_float32, f32);
    typed_rolling_mean!(rolling_mean_float64, f64);

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

    pub fn rank(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array, cmp_func] = assume::exactly_n(args);
        let array = assume::array(array, layout_args[0]);

        fn get_rank(start: usize, _end: usize, _group: usize) -> usize {
            start + 1
        }
        let ranks = rank_impl(it, array, &cmp_func, get_rank)?;

        Ok(Cell::Data(encode_primitives(
            ranks.into_iter().map(|r| r as i32),
        )))
    }

    pub fn rank_dense(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array, cmp_func] = assume::exactly_n(args);
        let array = assume::array(array, layout_args[0]);

        fn get_rank(_start: usize, _end: usize, group: usize) -> usize {
            group + 1
        }
        let ranks = rank_impl(it, array, &cmp_func, get_rank)?;

        Ok(Cell::Data(encode_primitives(
            ranks.into_iter().map(|r| r as i32),
        )))
    }

    pub fn rank_percentile(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array, cmp_func] = assume::exactly_n(args);
        let array = assume::array(array, layout_args[0]);

        fn get_rank(start: usize, _end: usize, _group: usize) -> usize {
            start
        }
        let n = (array.remaining() - 1) as f64;
        let ranks = rank_impl(it, array, &cmp_func, get_rank)?;

        Ok(Cell::Data(encode_primitives(
            ranks.into_iter().map(|r| r as f64 / n),
        )))
    }

    pub fn cume_dist(
        it: &mut Interpreter,
        layout_args: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [array, cmp_func] = assume::exactly_n(args);
        let array = assume::array(array, layout_args[0]);

        fn get_rank(_start: usize, end: usize, _group: usize) -> usize {
            end
        }
        let n = array.remaining() as f64;
        let ranks = rank_impl(it, array, &cmp_func, get_rank)?;

        Ok(Cell::Data(encode_primitives(
            ranks.into_iter().map(|r| r as f64 / n),
        )))
    }

    fn rank_impl(
        it: &mut Interpreter,
        array: ArrayReader<Data>,
        cmp: &Cell,
        get_rank: impl Fn(usize, usize, usize) -> usize,
    ) -> Result<Vec<usize>, EvalError> {
        let array_len = array.remaining();

        // sort using injected comparator
        let mut indexed: Vec<(usize, Data)> = array.enumerate().collect();
        let mut sort_err: Option<EvalError> = None;
        indexed.sort_by(|(_, a), (_, b)| {
            if sort_err.is_some() {
                return Ordering::Equal;
            }
            match eval_cmp(it, cmp, a, b) {
                Ok(ord) => ord,
                Err(e) => {
                    sort_err = Some(e);
                    Ordering::Equal
                }
            }
        });
        if let Some(e) = sort_err {
            return Err(e);
        }

        // iterate over groups
        let mut ranks = vec![0; array_len];
        let mut i = 0;
        let mut groups = 0;
        while i < array_len {
            // find group of equal values
            let start = i;
            let value = &indexed[i].1;
            while i < array_len {
                let ord = eval_cmp(it, cmp, &indexed[i].1, value)?;
                if !ord.is_eq() {
                    break;
                }
                i += 1;
            }
            let end = i; // exclusive

            // assign ranks to group
            let r = get_rank(start, end, groups);
            for (idx, _) in indexed.iter().take(end).skip(start) {
                ranks[*idx] = r;
            }
            groups += 1;
        }

        Ok(ranks)
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
                "split" => &split,
                "starts_with" => &starts_with,
                "contains" => &contains,
                "ends_with" => &ends_with,

                _ => return None,
            })
        }
    }

    pub fn concat(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
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

    pub fn length(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [text] = assume::exactly_n(args);

        let data = assume::into_data(text)?;
        let string = decode::text_ref(data);
        let length = string.as_str().chars().count() as u32;

        Ok(Cell::Data(encode(&length)))
    }

    pub fn from_ascii(
        _it: &mut Interpreter,
        _: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [ascii] = assume::exactly_n(args);
        let ascii = assume::primitive::<u8>(&ascii)?;

        let text = char::from(ascii).to_string();
        Ok(Cell::Data(encode(&text)))
    }

    pub fn join(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [parts, sep] = assume::exactly_n(args);
        let mut parts = assume::array(parts, str::head_size().div_ceil(8) as u32);
        let sep = assume::text(&sep)?;

        let joined = match parts.next() {
            None => String::new(),
            Some(first) => {
                let (lower, _) = parts.size_hint();

                let mut result = String::with_capacity(sep.len() * lower);
                result.push_str(decode::text_ref(first).as_str());
                for part in parts {
                    result.push_str(&sep);
                    result.push_str(decode::text_ref(part).as_str());
                }
                result
            }
        };
        Ok(Cell::Data(encode(&joined)))
    }

    pub fn split(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [text, sep] = assume::exactly_n(args);
        let text = assume::text_ref(text)?;
        let sep = assume::text_ref(sep)?;

        let mut output = crate::ArrayWriter::new(String::head_size().div_ceil(8) as u32, &[0]);
        for part in text.as_str().split(sep.as_str()) {
            output.write_item(Data::new(part.encode()));
        }
        Ok(Cell::Data(output.finish()))
    }

    pub fn starts_with(
        _it: &mut Interpreter,
        _: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [text, prefix] = assume::exactly_n(args);

        let text = assume::text_ref(text)?;
        let prefix = assume::text_ref(prefix)?;

        let res = text.as_str().starts_with(prefix.as_str());

        Ok(Cell::Data(encode(&res)))
    }

    pub fn contains(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [text, pattern] = assume::exactly_n(args);

        let text = assume::text_ref(text)?;
        let pattern = assume::text_ref(pattern)?;

        let res = text.as_str().contains(pattern.as_str());

        Ok(Cell::Data(encode(&res)))
    }

    pub fn ends_with(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [text, suffix] = assume::exactly_n(args);

        let text = assume::text_ref(text)?;
        let suffix = assume::text_ref(suffix)?;

        let res = text.as_str().ends_with(suffix.as_str());

        Ok(Cell::Data(encode(&res)))
    }
}

pub mod std_math {
    use crate::native::{assume, encode};
    use crate::{Cell, EvalError, Interpreter, NativeModule};

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "abs_int8" => &abs_int8,
                "abs_int16" => &abs_int16,
                "abs_int32" => &abs_int32,
                "abs_int64" => &abs_int64,
                "abs_float32" => &abs_float32,
                "abs_float64" => &abs_float64,

                "pow_int64" => &pow_int64,
                "pow_float64" => &pow_float64,

                _ => return None,
            })
        }
    }

    // Per-type abs
    macro_rules! typed_abs {
        ($name: ident, $prim: ty) => {
            pub fn $name(
                _: &mut Interpreter,
                _: &[u32],
                args: Vec<Cell>,
            ) -> Result<Cell, EvalError> {
                let [arg] = assume::exactly_n(args);
                Ok(Cell::Data(encode(&assume::primitive::<$prim>(&arg)?.abs())))
            }
        };
    }
    typed_abs!(abs_int8, i8);
    typed_abs!(abs_int16, i16);
    typed_abs!(abs_int32, i32);
    typed_abs!(abs_int64, i64);
    typed_abs!(abs_float32, f32);
    typed_abs!(abs_float64, f64);

    // Per-type pow (std.lt only defines pow for int64 | float64)
    pub fn pow_int64(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let (left, right) = assume::primitive2::<i64>(&args)?;
        Ok(Cell::Data(encode(&left.pow(right as u32))))
    }
    pub fn pow_float64(_: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let (left, right) = assume::primitive2::<f64>(&args)?;
        Ok(Cell::Data(encode(&left.powf(right))))
    }
}

pub mod std_fs {
    use ::std::{fs, io, path};

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
        let ty = assume::bytes(layout_args.next_slice());
        let ty = ir::Ty::decode(&ty).map_err(|_| EvalError::BadProgram)?;

        // init parquet reader
        let file = match fs::File::open(&file_path) {
            Ok(file) => file,
            Err(e) => {
                return Err(EvalError::ExternalError(format!(
                    "read_parquet({}): {e}",
                    file_path.display()
                )));
            }
        };
        let builder = ParquetRecordBatchReaderBuilder::try_new(file).unwrap();

        let reader = builder
            .build()
            .map_err(|e| EvalError::ExternalError(e.to_string()))?;
        let batches = reader
            .collect::<Result<_, _>>()
            .map_err(|e| EvalError::ExternalError(e.to_string()))?;

        let data = lutra_arrow::arrow_to_lutra(batches, &ty, &it.defs)
            .map_err(|e| EvalError::ExternalError(e.to_string()))?;

        Ok(Cell::Data(Data::new(data.to_vec())))
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
        let ty = assume::bytes(layout_args.next_slice());
        let ty = ir::Ty::decode(&ty).map_err(|_| EvalError::BadProgram)?;

        // convert lutra to arrow
        let batch = lutra_arrow::lutra_to_arrow(data, &ty, &it.defs)
            .map_err(|e| EvalError::ExternalError(format!("lutra_to_arrow: {e}")))?;

        // write to parquet
        let file = match fs::File::create(&file_path) {
            Ok(file) => file,
            Err(e) => {
                return Err(EvalError::ExternalError(format!("write_parquet: {e}")));
            }
        };
        let mut writer = io::BufWriter::new(file);
        let mut builder = ArrowWriter::try_new(&mut writer, batch.schema(), None).unwrap();
        builder.write(&batch).unwrap();
        builder.finish().unwrap();

        Ok(Cell::Data(Data::new(vec![])))
    }
}

pub mod std_date {
    use chrono::Datelike;

    use crate::{Data, EvalError, native::*};

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "to_timestamp" => &to_timestamp,
                "to_year_month_day" => &to_year_month_day,
                _ => return None,
            })
        }
    }

    pub fn to_timestamp(
        _it: &mut Interpreter,
        _: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        let [date, time_zone] = assume::exactly_n(args);

        let date: i32 = assume::primitive(&date)?;

        let time_zone = assume::text_ref(time_zone)?;

        // convert date to timestamp
        let timestamp = {
            use chrono::{NaiveDate, NaiveDateTime, NaiveTime, TimeZone};

            // TODO: handle bad timezone
            let tz: chrono_tz::Tz = time_zone.as_str().parse().unwrap();

            // TODO: handle unwrap
            let date = NaiveDate::from_epoch_days(date).unwrap();
            let t = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
            let datetime = NaiveDateTime::new(date, t);

            // TODO: handle bad conversion
            let datetime = tz.from_local_datetime(&datetime).unwrap();

            datetime.timestamp_micros()
        };

        Ok(Cell::Data(encode(&timestamp)))
    }

    pub fn to_year_month_day(
        _it: &mut Interpreter,
        _: &[u32],
        args: Vec<Cell>,
    ) -> Result<Cell, EvalError> {
        // unpack
        let [date] = assume::exactly_n(args);
        let date: i32 = assume::primitive(&date)?;

        // convert
        let date = chrono::NaiveDate::from_epoch_days(date).unwrap();
        let year: i32 = date.year();
        let month = date.month() as u8;
        let day = date.day() as u8;

        // encode
        let mut r = Vec::with_capacity(6);
        r.extend(year.encode());
        r.extend(month.encode());
        r.extend(day.encode());
        Ok(Cell::Data(Data::new(r)))
    }
}

pub mod std_timestamp {
    use crate::{EvalError, native::*};

    pub struct Module;

    impl NativeModule for Module {
        fn lookup_native_symbol(&self, id: &str) -> Option<crate::interpreter::NativeFunction> {
            Some(match id {
                "to_date" => &to_date,
                _ => return None,
            })
        }
    }

    pub fn to_date(_it: &mut Interpreter, _: &[u32], args: Vec<Cell>) -> Result<Cell, EvalError> {
        let [timestamp, time_zone] = assume::exactly_n(args);

        let timestamp: i64 = assume::primitive(&timestamp)?;
        let time_zone = assume::text_ref(time_zone)?;

        let date = {
            let tz: chrono_tz::Tz = time_zone.as_str().parse().unwrap();
            let datetime = chrono::DateTime::from_timestamp_micros(timestamp).unwrap();

            datetime.with_timezone(&tz).date_naive().to_epoch_days()
        };

        Ok(Cell::Data(encode(&date)))
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

    fn version(_it: &mut Interpreter, _: &[u32], _args: Vec<Cell>) -> Result<Cell, EvalError> {
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

    pub fn text_ref(cell: Cell) -> Result<decode::TextData, EvalError> {
        Ok(decode::text_ref(into_data(cell)?))
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
    use lutra_bin::{ArrayReader, Decode};

    use crate::Data;

    pub fn primitive<T: Decode>(data: &Data) -> T {
        T::decode(data.chunk()).unwrap()
    }

    pub fn bool(data: &Data) -> bool {
        bool::decode(data.chunk()).unwrap()
    }

    pub fn text(data: &Data) -> String {
        String::decode(&data.flatten()).unwrap()
    }

    pub fn text_ref(mut data: Data) -> TextData {
        let (offset, len) = ArrayReader::<&[u8]>::read_head(data.chunk());
        data.advance(offset);

        let body = data;
        TextData { body, len }
    }

    pub struct TextData {
        body: Data,
        len: usize,
    }

    impl TextData {
        pub fn as_str(&self) -> &str {
            str::from_utf8(&self.body.chunk()[..self.len]).unwrap()
        }
    }
}

pub fn encode<T: Encode + Layout + ?Sized>(value: &T) -> Data {
    Data::new(value.encode())
}

pub fn encode_option(value: Option<Data>, inner_head_bytes: u32) -> Data {
    let has_ptr = inner_head_bytes > 4;
    if has_ptr {
        match value {
            None => Data::new(vec![0; 5]),
            Some(x) => Data::new(vec![1, 4, 0, 0, 0]).combine(x),
        }
    } else {
        // inline
        match value {
            None => Data::new(vec![0; inner_head_bytes as usize + 1]),
            Some(x) => Data::new(vec![1]).combine(x),
        }
    }
}

pub fn encode_primitives<T: Encode + Layout + Sized>(values: impl Iterator<Item = T>) -> Data {
    let mut out = crate::ArrayWriter::new(T::head_size().div_ceil(8) as u32, &[]);
    for v in values {
        out.write_item(Data::new(v.encode()));
    }
    out.finish()
}

pub fn encode_cell<T: Encode + Layout + ?Sized>(value: &T) -> Cell {
    Cell::Data(encode(value))
}
