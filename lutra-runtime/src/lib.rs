mod interpreter;
mod native;
mod test;

pub use interpreter::evaluate;

#[test]
fn interpreter_layout() {
    insta::assert_snapshot!(std::mem::size_of::<interpreter::Cell>(), @"24");
}
