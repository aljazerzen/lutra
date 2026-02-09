//! Value and type name formatting for table cells.

use crate::ir;
use crate::{ArrayReader, Decode, Error, Result};

use super::Table;
use super::layout::Align;

/// Truncate string with `…` if over max_width.
pub fn truncate(s: &str, max_width: usize) -> String {
    if s.chars().count() <= max_width {
        s.to_string()
    } else {
        let truncated: String = s.chars().take(max_width - 1).collect();
        format!("{}…", truncated)
    }
}

/// Format a value for display, returning (text, alignment).
pub fn format_value(data: &[u8], ty: &ir::Ty, table: &Table) -> Result<(String, Align)> {
    // Try special types first
    if let Some(result) = format_special(data, ty) {
        return result;
    }

    format_value_impl(data, ty, table)
}

fn format_value_impl(data: &[u8], ty: &ir::Ty, table: &Table) -> Result<(String, Align)> {
    let ty = table.get_ty_mat(ty);
    match &ty.kind {
        ir::TyKind::Primitive(prim) => format_primitive(data, prim),
        ir::TyKind::Enum(variants) => format_enum(data, ty, variants, table),
        ir::TyKind::Array(_) => Ok(("[…]".into(), Align::Left)),
        ir::TyKind::Tuple(_) => Ok(("{…}".into(), Align::Left)),
        ir::TyKind::Function(_) => Ok(("fn".into(), Align::Left)),
        ir::TyKind::Ident(_) => unreachable!("should be resolved"),
    }
}

/// Format a primitive value, returning (text, alignment).
pub fn format_primitive(data: &[u8], prim: &ir::TyPrimitive) -> Result<(String, Align)> {
    match prim {
        ir::TyPrimitive::bool => {
            let v = bool::decode(data)?;
            Ok((if v { "true" } else { "false" }.into(), Align::Left))
        }
        ir::TyPrimitive::text => {
            let (offset, len) = ArrayReader::<&[u8]>::read_head(data);
            let text_data = &data[offset..offset + len];
            let s = std::str::from_utf8(text_data).map_err(|_| Error::InvalidData)?;
            Ok((s.to_string(), Align::Left))
        }
        ir::TyPrimitive::int8 => Ok((i8::decode(data)?.to_string(), Align::Right)),
        ir::TyPrimitive::int16 => Ok((i16::decode(data)?.to_string(), Align::Right)),
        ir::TyPrimitive::int32 => Ok((i32::decode(data)?.to_string(), Align::Right)),
        ir::TyPrimitive::int64 => Ok((i64::decode(data)?.to_string(), Align::Right)),
        ir::TyPrimitive::uint8 => Ok((u8::decode(data)?.to_string(), Align::Right)),
        ir::TyPrimitive::uint16 => Ok((u16::decode(data)?.to_string(), Align::Right)),
        ir::TyPrimitive::uint32 => Ok((u32::decode(data)?.to_string(), Align::Right)),
        ir::TyPrimitive::uint64 => Ok((u64::decode(data)?.to_string(), Align::Right)),
        ir::TyPrimitive::float32 => {
            let v = f32::decode(data)?;
            Ok((format_float(v as f64), Align::Right))
        }
        ir::TyPrimitive::float64 => {
            let v = f64::decode(data)?;
            Ok((format_float(v), Align::Right))
        }
    }
}

fn format_float(v: f64) -> String {
    if v.is_nan() {
        "NaN".into()
    } else if v.is_infinite() {
        if v.is_sign_positive() {
            "Inf".into()
        } else {
            "-Inf".into()
        }
    } else if v.fract() == 0.0 && v.abs() < 1e15 {
        format!("{:.1}", v)
    } else {
        format!("{}", v)
    }
}

/// Try to format special types (Date, Time, Timestamp, Decimal).
/// Returns None if not a special type.
#[cfg(feature = "chrono")]
pub fn format_special(data: &[u8], ty: &ir::Ty) -> Option<Result<(String, Align)>> {
    let ir::TyKind::Ident(path) = &ty.kind else {
        return None;
    };

    if path.0 == ["std", "Date"] {
        let days = match i32::decode(data) {
            Ok(d) => d,
            Err(e) => return Some(Err(e)),
        };
        if let Some(date) = chrono::NaiveDate::from_num_days_from_ce_opt(days) {
            Some(Ok((format!("{}", date), Align::Right)))
        } else {
            Some(Ok((days.to_string(), Align::Right)))
        }
    } else if path.0 == ["std", "Time"] {
        let micros_t = match i64::decode(data) {
            Ok(m) => m,
            Err(e) => return Some(Err(e)),
        };
        let micros = (micros_t % 1_000_000).unsigned_abs();
        let sec_t = micros_t / 1_000_000;
        let sec = (sec_t % 60).unsigned_abs();
        let min_t = sec_t / 60;
        let min = (min_t % 60).unsigned_abs();
        let h = min_t / 60;
        Some(Ok((
            format!("{:02}:{:02}:{:02}.{:06}", h, min, sec, micros),
            Align::Right,
        )))
    } else if path.0 == ["std", "Timestamp"] {
        let micros = match i64::decode(data) {
            Ok(m) => m,
            Err(e) => return Some(Err(e)),
        };
        if let Some(dt) = chrono::DateTime::from_timestamp_micros(micros) {
            let formatted = dt.format("%Y-%m-%dT%H:%M:%S%.6f").to_string();
            Some(Ok((formatted, Align::Right)))
        } else {
            Some(Ok((micros.to_string(), Align::Right)))
        }
    } else if path.0 == ["std", "Decimal"] {
        let val = match i64::decode(data) {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        Some(Ok((
            format!("{}.{:02}", val / 100, (val % 100).abs()),
            Align::Right,
        )))
    } else {
        None
    }
}

#[cfg(not(feature = "chrono"))]
pub fn format_special(_data: &[u8], _ty: &ir::Ty) -> Option<Result<(String, Align)>> {
    None
}

fn format_enum(
    data: &[u8],
    enum_ty: &ir::Ty,
    variants: &[ir::TyEnumVariant],
    table: &Table,
) -> Result<(String, Align)> {
    let (tag, inner_data) = read_enum_tag(data, enum_ty, variants)?;
    let variant = &variants[tag];

    let payload_ty = table.get_ty_mat(&variant.ty);
    let is_unit = matches!(&payload_ty.kind, ir::TyKind::Tuple(f) if f.is_empty());

    if is_unit {
        Ok((variant.name.clone(), Align::Left))
    } else if table.is_flat(&variant.ty) {
        let (inner_text, _) = format_value_impl(inner_data, &variant.ty, table)?;
        Ok((format!("{}({})", variant.name, inner_text), Align::Left))
    } else {
        Ok((format!("{}(…)", variant.name), Align::Left))
    }
}

fn read_enum_tag<'d>(
    data: &'d [u8],
    enum_ty: &ir::Ty,
    variants: &[ir::TyEnumVariant],
) -> Result<(usize, &'d [u8])> {
    let head = crate::layout::enum_head_format(variants, &enum_ty.variants_recursive);

    let mut tag_bytes = [0u8; 8];
    tag_bytes[0..head.tag_bytes as usize].copy_from_slice(&data[0..head.tag_bytes as usize]);
    let tag = u64::from_le_bytes(tag_bytes) as usize;

    let variant = variants.get(tag).ok_or(Error::InvalidData)?;
    let variant_format = crate::layout::enum_variant_format(&head, &variant.ty);

    let inner_data = if head.has_ptr {
        if variant_format.is_unit {
            &data[head.tag_bytes as usize..]
        } else {
            let ptr_start = head.tag_bytes as usize;
            let ptr =
                u32::from_le_bytes(data[ptr_start..ptr_start + 4].try_into().unwrap()) as usize;
            &data[ptr_start + ptr..]
        }
    } else {
        &data[head.tag_bytes as usize..]
    };

    Ok((tag, inner_data))
}

/// Format type name for header row.
pub fn format_ty_name(ty: &ir::Ty, table: &Table) -> String {
    let ty = table.get_ty_mat(ty);
    match &ty.kind {
        ir::TyKind::Primitive(p) => format_primitive_ty_name(p),
        ir::TyKind::Array(item) => {
            if table.is_flat(item) {
                format!("[{}]", format_ty_name(item, table))
            } else {
                "[…]".into()
            }
        }
        ir::TyKind::Enum(variants) => {
            let names: Vec<_> = variants.iter().map(|v| v.name.as_str()).collect();
            if names.len() <= 3 {
                format!("enum{{{}}}", names.join(","))
            } else {
                format!("enum{{{},…}}", names[..2].join(","))
            }
        }
        ir::TyKind::Ident(path) => {
            // Show last segment for special types (Date, Timestamp, etc.)
            path.0.last().cloned().unwrap_or_default()
        }
        ir::TyKind::Tuple(_) => "{…}".into(),
        ir::TyKind::Function(_) => "fn".into(),
    }
}

fn format_primitive_ty_name(prim: &ir::TyPrimitive) -> String {
    match prim {
        ir::TyPrimitive::bool => "bool",
        ir::TyPrimitive::int8 => "int8",
        ir::TyPrimitive::int16 => "int16",
        ir::TyPrimitive::int32 => "int32",
        ir::TyPrimitive::int64 => "int64",
        ir::TyPrimitive::uint8 => "uint8",
        ir::TyPrimitive::uint16 => "uint16",
        ir::TyPrimitive::uint32 => "uint32",
        ir::TyPrimitive::uint64 => "uint64",
        ir::TyPrimitive::float32 => "float32",
        ir::TyPrimitive::float64 => "float64",
        ir::TyPrimitive::text => "text",
    }
    .into()
}
