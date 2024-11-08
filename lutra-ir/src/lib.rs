mod literal;
mod parser;
mod printer;
mod sid;

pub mod ir {
    pub use crate::sid::SidKind;

    include!(concat!(env!("OUT_DIR"), "/ir.rs"));
}

pub use parser::{_test_parse, parse};
pub use printer::print;
