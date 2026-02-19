use lutra_bin::ir;
use sql_ast as sa;

use crate::sql::Dialect;

impl super::Context<'_> {
    /// Compile integer cast functions (std::to_int8, std::to_uint32, etc.)
    pub(super) fn compile_to_int(
        &self,
        expr: sa::Expr,
        source: &ir::Ty,
        target: &ir::Ty,
    ) -> sa::Expr {
        let target_ty_name = self.ty_name(target);

        let source = source.kind.as_primitive().unwrap();
        let target = target.kind.as_primitive().unwrap();

        match source {
            ir::TyPrimitive::int8
            | ir::TyPrimitive::int16
            | ir::TyPrimitive::int32
            | ir::TyPrimitive::int64
            | ir::TyPrimitive::uint8
            | ir::TyPrimitive::uint16
            | ir::TyPrimitive::uint32
            | ir::TyPrimitive::uint64 => {
                let expr = match self.dialect {
                    // PostgreSQL casts implement wrapping already
                    Dialect::Postgres => expr,
                    // DuckDB needs manual wrapping impl
                    Dialect::DuckDB => self.compile_to_int_wrapping(expr, *source, *target),
                };

                sa::Expr::Source(format!("({expr})::{target_ty_name}"))
            }
            ir::TyPrimitive::float32 | ir::TyPrimitive::float64 => {
                sa::Expr::Source(format!("trunc({expr})::{target_ty_name}"))
            }
            _ => panic!("unexpected source type for integer cast: {:?}", source),
        }
    }

    fn compile_to_int_wrapping(
        &self,
        expr: sa::Expr,
        source: ir::TyPrimitive,
        target: ir::TyPrimitive,
    ) -> sa::Expr {
        // get bit width and signedness for source and target
        let (t_bits, t_signed) = match target {
            ir::TyPrimitive::int8 => (8, true),
            ir::TyPrimitive::int16 => (16, true),
            ir::TyPrimitive::int32 => (32, true),
            ir::TyPrimitive::int64 => (64, true),
            ir::TyPrimitive::uint8 => (8, false),
            ir::TyPrimitive::uint16 => (16, false),
            ir::TyPrimitive::uint32 => (32, false),
            ir::TyPrimitive::uint64 => (64, false),
            _ => return expr,
        };

        let (s_bits, s_signed) = match source {
            ir::TyPrimitive::int8 => (8, true),
            ir::TyPrimitive::int16 => (16, true),
            ir::TyPrimitive::int32 => (32, true),
            ir::TyPrimitive::int64 => (64, true),
            ir::TyPrimitive::uint8 => (8, false),
            ir::TyPrimitive::uint16 => (16, false),
            ir::TyPrimitive::uint32 => (32, false),
            ir::TyPrimitive::uint64 => (64, false),
            _ => return expr,
        };

        // check if we need wrapping behavior
        let needs_wrapping = s_bits > t_bits
            || (s_signed && !t_signed)
            || (!s_signed && t_signed && s_bits >= t_bits);

        if !needs_wrapping {
            return expr;
        }

        // implement wrapping using modulo arithmetic
        if t_signed {
            // casting to signed: wrap to unsigned range first, then reinterpret as signed
            let modulo = 1u128 << t_bits;
            let half = 1i128 << (t_bits - 1);

            // TODO: we don't want to use expr multiple times, it should only be used once.
            sa::Expr::Source(format!(
                "CASE WHEN (({expr})::INT8 % {modulo} + {modulo}) % {modulo} >= {half} THEN ((({expr})::INT8 % {modulo} + {modulo}) % {modulo}) - {modulo} ELSE (({expr})::INT8 % {modulo} + {modulo}) % {modulo} END"
            ))
        } else {
            // casting to unsigned: ((value % modulo) + modulo) % modulo
            let modulo = 1u128 << t_bits;
            sa::Expr::Source(format!("(({expr})::INT8 % {modulo} + {modulo}) % {modulo}"))
        }
    }

    pub(super) fn compile_literal(&self, lit: &ir::Literal, ty: &ir::Ty) -> sa::Expr {
        sa::Expr::Source(match lit {
            ir::Literal::bool(true) => "TRUE".to_string(),
            ir::Literal::bool(false) => "FALSE".to_string(),
            ir::Literal::int8(v) => {
                if *v >= 0 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("({v})::{}", self.ty_name(ty))
                }
            }
            ir::Literal::int16(v) => {
                if *v >= 0 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("({v})::{}", self.ty_name(ty))
                }
            }
            ir::Literal::int32(v) => {
                if *v >= 0 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("({v})::{}", self.ty_name(ty))
                }
            }
            ir::Literal::int64(v) => {
                if *v >= 0 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("({v})::{}", self.ty_name(ty))
                }
            }
            ir::Literal::uint8(v) => {
                if self.dialect == Dialect::DuckDB || *v < 0x80 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("{}::{}", *v as i8, self.ty_name(ty))
                }
            }
            ir::Literal::uint16(v) => {
                if self.dialect == Dialect::DuckDB || *v < 0x8000 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("{}::{}", *v as i16, self.ty_name(ty))
                }
            }
            ir::Literal::uint32(v) => {
                if self.dialect == Dialect::DuckDB || *v < 0x80000000 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("{}::{}", *v as i32, self.ty_name(ty))
                }
            }
            ir::Literal::uint64(v) => {
                if self.dialect == Dialect::DuckDB || *v < 0x8000000000000000 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("{}::{}", *v as i64, self.ty_name(ty))
                }
            }
            ir::Literal::float32(v) => format!("{v}::{}", self.ty_name(ty)),
            ir::Literal::float64(v) => format!("{v}::{}", self.ty_name(ty)),
            ir::Literal::text(s) => {
                let escaped = sa::escape_string(s, '\'');
                return sa::Expr::Source(format!("'{escaped}'::text"));
            }
        })
    }
}
