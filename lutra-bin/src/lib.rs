#![cfg_attr(not(feature = "std"), no_std)]

pub mod br;
mod data;
mod decode;
mod encode;
mod error;
pub mod ir;
pub mod layout;
pub mod reader;
mod value;
mod writer;

#[path = "project/generated.rs"]
mod generated;

pub use data::Data;
pub use decode::Decode;
pub use encode::{Encode, ReversePointer};
pub use error::{Error, Result};
pub use layout::Layout;
pub use reader::{ArrayReader, EnumReader, ReaderExt, TupleReader};
pub use value::Value;
pub use writer::{ArrayWriter, EnumWriter, TupleWriter};

pub use bytes;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
pub use alloc::vec;
#[cfg(feature = "std")]
pub use std::vec;

#[cfg(not(feature = "std"))]
pub use alloc::string;
#[cfg(feature = "std")]
pub use std::string;

#[cfg(not(feature = "std"))]
use alloc::borrow;
#[cfg(feature = "std")]
use std::borrow;

#[cfg(not(feature = "std"))]
use alloc::rc;
#[cfg(feature = "std")]
use std::rc;

#[cfg(not(feature = "std"))]
pub use alloc::boxed;
#[cfg(feature = "std")]
pub use std::boxed;

#[cfg(not(feature = "std"))]
pub use alloc::collections;
#[cfg(feature = "std")]
pub use std::collections;
