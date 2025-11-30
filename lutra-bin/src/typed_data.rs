//! Typed data format
//!
//! Utilities for combinding data with types into a single serialized buffer.
//!
//! It is serialized with the following schema:
//! ```lt
//! type TypedData: {ty: ir::Ty, ty_defs: [ir::TyDef], data: [uint8]}
//! ```

use crate::ir;
use crate::vec;
use crate::{ArrayReader, Decode, Encode, Layout, Result};

pub fn decode(buffer: &[u8]) -> Result<(&[u8], ir::Ty, vec::Vec<ir::TyDef>)> {
    let ty = ir::Ty::decode(buffer)?;
    let field_size = ir::Ty::head_size().div_ceil(8);
    let buffer = &buffer[field_size..];

    let ty_defs: vec::Vec<ir::TyDef> = Decode::decode(buffer)?;
    let field_size = <vec::Vec<ir::TyDef> as Layout>::head_size().div_ceil(8);
    let buffer = &buffer[field_size..];

    let (offset, len) = ArrayReader::<&[u8]>::read_head(buffer);
    let data = &buffer[offset..offset + len];

    Ok((data, ty, ty_defs))
}

pub fn encode(
    buffer: &mut bytes::BytesMut,
    data: &[u8],
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> Result<()> {
    let ty_res = ty.encode_head(buffer);
    let ty_defs_res = ty_defs.encode_head(buffer);
    let data_res = data.encode_head(buffer);

    ty.encode_body(ty_res, buffer);
    ty_defs.encode_body(ty_defs_res, buffer);
    data.encode_body(data_res, buffer);

    Ok(())
}
