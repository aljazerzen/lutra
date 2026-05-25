use lutra_bin::ir;
use lutra_sql as sa;

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

        let source = self.get_ty_std(source).unwrap();
        let target = self.get_ty_std(target).unwrap();

        if source.is_float() {
            return sa::Expr::Source(format!("trunc({expr})::{target_ty_name}"));
        }

        let expr = match self.dialect {
            // PostgreSQL casts implement wrapping already
            Dialect::Postgres => expr,
            // DuckDB needs manual wrapping impl
            Dialect::DuckDB => self.compile_to_int_wrapping(expr, source, target),
        };

        sa::Expr::Source(format!("({expr})::{target_ty_name}"))
    }

    fn compile_to_int_wrapping(
        &self,
        expr: sa::Expr,
        source: ir::TyStd,
        target: ir::TyStd,
    ) -> sa::Expr {
        if target.is_float() || source.is_float() {
            return expr;
        }

        // get bit width and signed-ness for source and target
        let (t_bits, t_signed) = (target.bits(), target.is_signed_int());
        let (s_bits, s_signed) = (source.bits(), source.is_signed_int());
        assert!(t_bits > 0);
        assert!(s_bits > 0);

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

            sa::Expr::Source(format!(
                "(SELECT CASE WHEN w >= {half} THEN w - {modulo} ELSE w END FROM (SELECT (({expr})::INT8 % {modulo} + {modulo}) % {modulo} AS w) t)"
            ))
        } else {
            // casting to unsigned: ((value % modulo) + modulo) % modulo
            let modulo = 1u128 << t_bits;
            sa::Expr::Source(format!("(({expr})::INT8 % {modulo} + {modulo}) % {modulo}"))
        }
    }

    pub(super) fn compile_literal(&self, lit: &ir::Literal, ty: &ir::Ty) -> sa::Expr {
        let ty = self.get_ty_mat(ty);
        let ty_std = match &ty.kind {
            ir::TyKind::Primitive(ir::TyPrimitive::prim8) => ir::TyStd::Int8,
            ir::TyKind::Primitive(ir::TyPrimitive::prim16) => ir::TyStd::Int16,
            ir::TyKind::Primitive(ir::TyPrimitive::prim32) => ir::TyStd::Int32,
            ir::TyKind::Primitive(ir::TyPrimitive::prim64) => ir::TyStd::Int64,
            ir::TyKind::Ident(path) => ir::TyStd::try_new(path).unwrap(),
            _ => panic!("bad literal type"),
        };

        sa::Expr::Source(match ty_std {
            ir::TyStd::Bool => {
                let v = *lit.as_prim8().unwrap() != 0;
                if v { "TRUE" } else { "FALSE" }.to_string()
            }
            ir::TyStd::Int8 => {
                let v = *lit.as_prim8().unwrap() as i8;
                if v >= 0 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("({v})::{}", self.ty_name(ty))
                }
            }
            ir::TyStd::Int16 => {
                let v = *lit.as_prim16().unwrap() as i16;
                if v >= 0 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("({v})::{}", self.ty_name(ty))
                }
            }
            ir::TyStd::Int32 | ir::TyStd::Date => {
                let v = *lit.as_prim32().unwrap() as i32;
                if v >= 0 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("({v})::{}", self.ty_name(ty))
                }
            }
            ir::TyStd::Int64 | ir::TyStd::Time | ir::TyStd::Timestamp | ir::TyStd::Decimal => {
                let v = *lit.as_prim64().unwrap() as i64;
                if v >= 0 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("({v})::{}", self.ty_name(ty))
                }
            }
            ir::TyStd::UInt8 => {
                let v = lit.as_prim8().unwrap();
                if self.dialect == Dialect::DuckDB || *v < 0x80 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("{}::{}", *v as i8, self.ty_name(ty))
                }
            }
            ir::TyStd::UInt16 => {
                let v = lit.as_prim16().unwrap();
                if self.dialect == Dialect::DuckDB || *v < 0x8000 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("{}::{}", *v as i16, self.ty_name(ty))
                }
            }
            ir::TyStd::UInt32 => {
                let v = lit.as_prim32().unwrap();
                if self.dialect == Dialect::DuckDB || *v < 0x80000000 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("{}::{}", *v as i32, self.ty_name(ty))
                }
            }
            ir::TyStd::UInt64 => {
                let v = lit.as_prim64().unwrap();
                if self.dialect == Dialect::DuckDB || *v < 0x8000000000000000 {
                    format!("{v}::{}", self.ty_name(ty))
                } else {
                    format!("{}::{}", *v as i64, self.ty_name(ty))
                }
            }
            ir::TyStd::Float32 => {
                let v = f32::from_bits(*lit.as_prim32().unwrap());
                format!("{v}::{}", self.ty_name(ty))
            }
            ir::TyStd::Float64 => {
                let v = f64::from_bits(*lit.as_prim64().unwrap());
                format!("{v}::{}", self.ty_name(ty))
            }
            ir::TyStd::Text => {
                let s = lit.as_text().unwrap();
                let escaped = sa::escape_string(s, '\'');
                return sa::Expr::Source(format!("'{escaped}'::text"));
            }
        })
    }
}
