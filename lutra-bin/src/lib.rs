#![cfg_attr(not(feature = "std"), no_std)]

pub mod br;
mod decode;
mod encode;
pub mod error;
pub mod ident;
pub mod ir;
pub mod layout;
mod printer;
pub mod reader;
pub mod rr;
mod shape;
#[path = "std_types.rs"]
pub mod std;
mod tabular;
pub mod typed_data;
mod value;
mod visitor;

#[path = "project/generated.rs"]
#[allow(unused_imports)]
mod generated;

pub use decode::{Decode, decode_enum_head};
pub use encode::{Encode, ReversePointer, encode_enum_head_padding, encode_enum_head_tag};
pub use error::{Error, Result};
pub use layout::Layout;
pub use reader::{ArrayReader, ReaderExt, TupleReader};
pub use value::{Value, ValueVisitor};
pub use visitor::Visitor;

#[cfg(feature = "std")]
pub use printer::{print_duration, print_source, print_time};
pub use shape::{Shape, get_shape};
#[cfg(feature = "std")]
pub use tabular::{TableCell, TabularReader};

pub use bytes;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(feature = "std")]
pub use ::std::vec;
#[cfg(not(feature = "std"))]
pub use alloc::vec;

#[cfg(feature = "std")]
pub use ::std::string;
#[cfg(not(feature = "std"))]
pub use alloc::string;

#[cfg(feature = "std")]
use ::std::borrow;
#[cfg(not(feature = "std"))]
use alloc::borrow;

#[cfg(feature = "std")]
pub use ::std::boxed;
#[cfg(not(feature = "std"))]
pub use alloc::boxed;

#[cfg(feature = "std")]
pub use ::std::collections;
#[cfg(not(feature = "std"))]
pub use alloc::collections;
