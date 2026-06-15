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
    let ty_mat = table.get_ty_mat(ty);
    match &ty_mat.kind {
        ir::TyKind::Primitive(prim) => format_primitive(data, prim),
        ir::TyKind::Ident(ident) => format_ty_std(data, ir::TyStd::try_new(ident).unwrap()),
        ir::TyKind::Enum(variants) => format_enum(data, ty_mat, variants, table),
        ir::TyKind::Array(_) => Ok("[…]".into()),
        ir::TyKind::Tuple(_) => Ok("{…}".into()),
        ir::TyKind::Function(_) => Ok("func".into()),
    }
}

/// Format a primitive value, returning (text, alignment).
pub fn format_primitive(data: &[u8], prim: &ir::TyPrimitive) -> Result<String> {
    match prim {
        ir::TyPrimitive::Prim8 => Ok(i8::decode(data)?.to_string()),
        ir::TyPrimitive::Prim16 => Ok(i16::decode(data)?.to_string()),
        ir::TyPrimitive::Prim32 => Ok(i32::decode(data)?.to_string()),
        ir::TyPrimitive::Prim64 => Ok(i64::decode(data)?.to_string()),
    }
}

#[allow(dead_code)]
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
pub fn format_ty_std(data: &[u8], ty: ir::TyStd) -> Result<String> {
    Ok(match ty {
        ir::TyStd::Bool => {
            let v = bool::decode(data)?;
            if v { "true" } else { "false" }.into()
        }
        ir::TyStd::Int8 => i8::decode(data)?.to_string(),
        ir::TyStd::Int16 => i16::decode(data)?.to_string(),
        ir::TyStd::Int32 => i32::decode(data)?.to_string(),
        ir::TyStd::Int64 => i64::decode(data)?.to_string(),
        ir::TyStd::UInt8 => u8::decode(data)?.to_string(),
        ir::TyStd::UInt16 => u16::decode(data)?.to_string(),
        ir::TyStd::UInt32 => u32::decode(data)?.to_string(),
        ir::TyStd::UInt64 => u64::decode(data)?.to_string(),
        ir::TyStd::Float32 => {
            let v = f32::decode(data)?;
            format_float(v as f64)
        }
        ir::TyStd::Float64 => {
            let v = f64::decode(data)?;
            format_float(v)
        }
        ir::TyStd::Text => {
            let (offset, len) = ArrayReader::<&[u8]>::read_head(data);
            let text_data = &data[offset..offset + len];
            let s = std::str::from_utf8(text_data).map_err(|_| Error::InvalidData)?;
            s.to_string()
        }
        ir::TyStd::Date => {
            let days = i32::decode(data)?;
            if let Some(date) = chrono::NaiveDate::from_num_days_from_ce_opt(days) {
                format!("{}", date)
            } else {
                days.to_string()
            }
        }
        ir::TyStd::Duration => lutra_bin::print_duration(i64::decode(data)?),
        ir::TyStd::Time => lutra_bin::print_time(u64::decode(data)?),
        ir::TyStd::Timestamp => {
            let micros = i64::decode(data)?;
            if let Some(dt) = chrono::DateTime::from_timestamp_micros(micros) {
                dt.format("%Y-%m-%d %H:%M:%S%.6f").to_string()
            } else {
                micros.to_string()
            }
        }
        ir::TyStd::Decimal => {
            let val = i64::decode(data)?;
            format!("{}.{:02}", val / 100, (val % 100).abs())
        }
    })
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
        ir::TyKind::Primitive(p) => p.name().to_string(),
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
        ir::TyKind::Ident(path) => path.0.last().cloned().unwrap_or_default(),
        ir::TyKind::Tuple(_) => "{…}".into(),
        ir::TyKind::Function(_) => "fn".into(),
    }
}
