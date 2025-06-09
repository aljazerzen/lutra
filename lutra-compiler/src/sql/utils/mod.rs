#![allow(dead_code)]

mod ast;
mod expr_or_source;
mod projection;
mod scoped;

pub use ast::*;
pub use expr_or_source::*;
pub use scoped::*;

pub fn retain_by_position<T>(vec: &mut Vec<T>, to_keep: &[usize]) {
    let mut to_keep = to_keep.to_vec();
    to_keep.sort();
    to_keep.dedup();

    for (i, k) in to_keep.iter().enumerate() {
        if i < *k {
            vec.swap(i, *k);
        }
    }
    vec.truncate(to_keep.len());
}

pub fn drop_by_position<T>(vec: &mut Vec<T>, to_drop: &[usize]) {
    if to_drop.is_empty() {
        return;
    }

    let mut to_drop = to_drop.to_vec();
    to_drop.sort();
    to_drop.dedup();

    let n_to_keep = vec.len() - to_drop.len();

    let mut to_drop = to_drop.iter().peekable();
    let mut next_keep = 0;
    for i in 0..n_to_keep {
        while to_drop.peek().is_some_and(|d| **d == next_keep) {
            next_keep += 1;
            to_drop.next();
        }

        if i < next_keep {
            vec.swap(i, next_keep);
        }
        next_keep += 1;
    }
    vec.truncate(n_to_keep);
}

#[test]
fn drop_by_position_01() {
    let mut vec = vec!["a", "b", "c"];
    drop_by_position(&mut vec, &[0]);
    assert_eq!(vec, ["b", "c"])
}
