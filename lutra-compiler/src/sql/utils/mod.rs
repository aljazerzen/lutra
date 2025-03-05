#![allow(dead_code)]

mod ast;
mod expr_or_source;
mod projection;

pub use ast::*;
pub use expr_or_source::*;
pub use projection::*;

pub fn retain_by_position<T>(vec: &mut Vec<T>, mut to_keep: Vec<usize>) {
    to_keep.sort();
    to_keep.dedup();

    for (i, k) in to_keep.iter().enumerate() {
        if i < *k {
            vec.swap(i, *k);
        }
    }
    vec.truncate(to_keep.len());
}

pub fn drop_by_position<T>(vec: &mut Vec<T>, mut to_drop: Vec<usize>) {
    if to_drop.is_empty() {
        return;
    }

    to_drop.sort();
    to_drop.dedup();

    let n_to_keep = vec.len() - to_drop.len();

    let mut to_drop = to_drop.iter().peekable();
    let mut next_keep = 0;
    for i in 0..n_to_keep {
        while to_drop.peek().map_or(false, |d| **d == next_keep) {
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
    drop_by_position(&mut vec, vec![0]);
    assert_eq!(vec, ["b", "c"])
}
