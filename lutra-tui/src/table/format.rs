//! Value and type name formatting for table cells.

use lutra_bin::ir;
use lutra_bin::{ArrayReader, Decode, Error, Result};

use super::Table;

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
pub fn format_value(data: &[u8], ty: &ir::Ty, table: &Table) -> Result<String> {
    // Try special types first (Date, Time, Timestamp, Decimal)
    if let Some(result) = format_special(data, ty) {
        return result;
    }

    let ty_mat = table.get_ty_mat(ty);
    match &ty_mat.kind {
        ir::TyKind::Primitive(prim) => format_primitive(data, prim),
        ir::TyKind::Enum(variants) => format_enum(data, ty_mat, variants, table),
        ir::TyKind::Array(_) => Ok("[…]".into()),
        ir::TyKind::Tuple(_) => Ok("{…}".into()),
        ir::TyKind::Function(_) => Ok("fn".into()),
        ir::TyKind::Ident(_) => unreachable!("should be resolved"),
    }
}

/// Format a primitive value, returning (text, alignment).
pub fn format_primitive(data: &[u8], prim: &ir::TyPrimitive) -> Result<String> {
    match prim {
        ir::TyPrimitive::bool => {
            let v = bool::decode(data)?;
            Ok(if v { "true" } else { "false" }.into())
        }
        ir::TyPrimitive::text => {
            let (offset, len) = ArrayReader::<&[u8]>::read_head(data);
            let text_data = &data[offset..offset + len];
            let s = std::str::from_utf8(text_data).map_err(|_| Error::InvalidData)?;
            Ok(s.to_string())
        }
        ir::TyPrimitive::int8 => Ok(i8::decode(data)?.to_string()),
        ir::TyPrimitive::int16 => Ok(i16::decode(data)?.to_string()),
        ir::TyPrimitive::int32 => Ok(i32::decode(data)?.to_string()),
        ir::TyPrimitive::int64 => Ok(i64::decode(data)?.to_string()),
        ir::TyPrimitive::uint8 => Ok(u8::decode(data)?.to_string()),
        ir::TyPrimitive::uint16 => Ok(u16::decode(data)?.to_string()),
        ir::TyPrimitive::uint32 => Ok(u32::decode(data)?.to_string()),
        ir::TyPrimitive::uint64 => Ok(u64::decode(data)?.to_string()),
        ir::TyPrimitive::float32 => {
            let v = f32::decode(data)?;
            Ok(format_float(v as f64))
        }
        ir::TyPrimitive::float64 => {
            let v = f64::decode(data)?;
            Ok(format_float(v))
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
pub fn format_special(data: &[u8], ty: &ir::Ty) -> Option<Result<String>> {
    let ir::TyKind::Ident(path) = &ty.kind else {
        return None;
    };

    if path.0 == ["std", "Date"] {
        let days = match i32::decode(data) {
            Ok(d) => d,
            Err(e) => return Some(Err(e)),
        };
        if let Some(date) = chrono::NaiveDate::from_num_days_from_ce_opt(days) {
            Some(Ok(format!("{}", date)))
        } else {
            Some(Ok(days.to_string()))
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
        Some(Ok(format!("{:02}:{:02}:{:02}.{:06}", h, min, sec, micros)))
    } else if path.0 == ["std", "Timestamp"] {
        let micros = match i64::decode(data) {
            Ok(m) => m,
            Err(e) => return Some(Err(e)),
        };
        if let Some(dt) = chrono::DateTime::from_timestamp_micros(micros) {
            let formatted = dt.format("%Y-%m-%d %H:%M:%S%.6f").to_string();
            Some(Ok(formatted))
        } else {
            Some(Ok(micros.to_string()))
        }
    } else if path.0 == ["std", "Decimal"] {
        let val = match i64::decode(data) {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        Some(Ok(format!("{}.{:02}", val / 100, (val % 100).abs())))
    } else {
        None
    }
}

fn format_enum(
    data: &[u8],
    enum_ty: &ir::Ty,
    variants: &[ir::TyEnumVariant],
    table: &Table,
) -> Result<String> {
    let (tag, inner_data) = read_enum_tag(data, enum_ty, variants)?;

    // special case: option enum
    if let Some(inner_ty) = enum_ty.kind.as_option() {
        return if tag == 0 {
            Ok("".into())
        } else {
            format_value(inner_data, inner_ty, table)
        };
    }

    let variant = &variants[tag];

    let payload_ty = table.get_ty_mat(&variant.ty);
    let is_unit = matches!(&payload_ty.kind, ir::TyKind::Tuple(f) if f.is_empty());

    if is_unit {
        Ok(variant.name.clone())
    } else if table.is_flat(&variant.ty) {
        let inner_text = format_value(inner_data, &variant.ty, table)?;
        Ok(format!("{}({})", variant.name, inner_text))
    } else {
        Ok(format!("{}(…)", variant.name))
    }
}

fn read_enum_tag<'d>(
    data: &'d [u8],
    enum_ty: &ir::Ty,
    variants: &[ir::TyEnumVariant],
) -> Result<(usize, &'d [u8])> {
    let head = lutra_bin::layout::enum_head_format(variants, &enum_ty.variants_recursive);

    let mut tag_bytes = [0u8; 8];
    tag_bytes[0..head.tag_bytes as usize].copy_from_slice(&data[0..head.tag_bytes as usize]);
    let tag = u64::from_le_bytes(tag_bytes) as usize;

    let variant = variants.get(tag).ok_or(Error::InvalidData)?;
    let variant_format = lutra_bin::layout::enum_variant_format(&head, &variant.ty);

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
    if let Some(inner) = ty.kind.as_option() {
        let inner = format_ty_name(inner, table);
        return format!("{inner}?");
    }

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
