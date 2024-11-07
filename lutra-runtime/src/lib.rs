mod interpreter;
mod literal;
mod native;
mod parser;
mod test;

mod ir {
    include!(concat!(env!("OUT_DIR"), "/ir.rs"));
}

pub use interpreter::evaluate;
pub use parser::parse_source as parse;

#[test]
fn interpreter_layout() {
    insta::assert_snapshot!(std::mem::size_of::<interpreter::Cell>(), @"24");
}
