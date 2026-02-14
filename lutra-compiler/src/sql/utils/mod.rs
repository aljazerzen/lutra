#![allow(dead_code)]

mod ast;
mod node;
mod projection;

use std::rc::Rc;

pub use ast::*;
pub use node::*;
pub use projection::RelCols;

pub fn pick_by_position<T: Clone>(vec: &mut Vec<T>, to_keep: &[usize]) {
    let rc: Vec<_> = vec.drain(..).map(Rc::new).collect();

    let mut rc_picked = Vec::with_capacity(to_keep.len());
    for k in to_keep {
        rc_picked.push(rc[*k].clone());
    }
    drop(rc);

    fn into_inner_or_clone<T: Clone>(r: Rc<T>) -> T {
        Rc::try_unwrap(r).unwrap_or_else(|r| r.as_ref().clone())
    }
    vec.extend(rc_picked.into_iter().map(into_inner_or_clone));
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

#[test]
fn pick_by_position_01() {
    let mut vec = vec!["a", "b", "c"];
    pick_by_position(&mut vec, &[1, 2]);
    assert_eq!(vec, ["b", "c"])
}

#[test]
fn pick_by_position_02() {
    let mut vec = vec!["a", "b", "c"];
    pick_by_position(&mut vec, &[0, 2]);
    assert_eq!(vec, ["a", "c"])
}

#[test]
fn pick_by_position_03() {
    let mut vec = vec!["a", "b", "c"];
    pick_by_position(&mut vec, &[2, 0]);
    assert_eq!(vec, ["c", "a"])
}

#[test]
fn pick_by_position_04() {
    let mut vec = vec!["a", "b", "c"];
    pick_by_position(&mut vec, &[0, 2, 0]);
    assert_eq!(vec, ["a", "c", "a"])
}
