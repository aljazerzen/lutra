use crate::sql::Dialect;
use crate::sql::queries::Context;
use crate::sql::utils::RelCols;
use lutra_bin::ir;

use super::utils;

impl<'p> Context<'p> {
    pub(crate) fn get_ty_std(&self, ty: &ir::Ty) -> Option<ir::TyStd> {
        let mut ty = ty;
        while let ir::TyKind::Ident(ident) = &ty.kind {
            if let Some(std) = ir::TyStd::try_new(ident) {
                return Some(std);
            }
            ty = self.types.get(ident).unwrap();
        }
        None
    }

    /// Get SQL type name for a Lutra type (in query repr)
    pub(super) fn ty_name(&self, ty: &ir::Ty) -> String {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Ident(path) => self
                .ty_std_name(ir::TyStd::try_new(path).unwrap())
                .to_string(),
            ir::TyKind::Primitive(prim) => {
                let ty = match prim {
                    ir::TyPrimitive::prim8 => ir::TyStd::Int8,
                    ir::TyPrimitive::prim16 => ir::TyStd::Int16,
                    ir::TyPrimitive::prim32 => ir::TyStd::Int32,
                    ir::TyPrimitive::prim64 => ir::TyStd::Int64,
                };
                self.ty_std_name(ty).to_string()
            }

            ir::TyKind::Tuple(fields) if fields.is_empty() => {
                // unit type (holds no data) does not exist in sql so we use a type with
                // the least amount of data
                "bool".to_string()
            }

            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                // option enum is a nullable column
                self.ty_name(&variants[1].ty)
            }

            ir::TyKind::Tuple(_) | ir::TyKind::Array(_) | ir::TyKind::Enum(_) => {
                // container types are serialized
                // (tuple should probably be flattened, but we probably never call
                //  this method on non-flattened expressions)
                self.ser_ty_name(ty)
            }

            ir::TyKind::Function(_) => unreachable!(),
        }
    }

    pub(super) fn ty_std_name(&self, ty: ir::TyStd) -> &'static str {
        use ir::TyStd::*;
        match self.dialect() {
            Dialect::Postgres => match ty {
                Bool => "bool",
                Int8 | Int16 | UInt8 | UInt16 => "int2",
                Int32 | UInt32 => "int4",
                Int64 | UInt64 => "int8",
                Float32 => "float4",
                Float64 => "float8",
                Text => "text",
                Date => "int4",
                Time | Timestamp | Decimal => "int8",
            },
            Dialect::DuckDB => match ty {
                Bool => "BOOL",
                Int8 => "INT1",
                Int16 => "INT2",
                Int32 => "INT4",
                Int64 => "INT8",
                UInt8 => "UINT8",
                UInt16 => "UINT16",
                UInt32 => "UINT32",
                UInt64 => "UINT64",
                Float32 => "FLOAT4",
                Float64 => "FLOAT8",
                Text => "TEXT",
                Date => "INT4",
                Time | Timestamp | Decimal => "INT8",
            },
        }
    }

    /// Get SQL type name in the "serialized repr"
    fn ser_ty_name(&self, ty: &ir::Ty) -> String {
        match self.dialect() {
            Dialect::Postgres => {
                // PostgreSQL uses jsonb for complex types
                "jsonb".to_string()
            }
            Dialect::DuckDB => {
                // DuckDB serializes to STRUCT/UNION/LIST
                self.duck_ty_name(ty)
            }
        }
    }

    /// Get type name for a Lutra type in "duckdb repr"
    pub(super) fn duck_ty_name(&self, ty: &ir::Ty) -> String {
        if let Some(ty) = self.get_ty_std(ty) {
            return self.duck_ty_std_name(ty).to_string();
        }

        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(prim) => {
                let ty = match prim {
                    ir::TyPrimitive::prim8 => ir::TyStd::Int8,
                    ir::TyPrimitive::prim16 => ir::TyStd::Int16,
                    ir::TyPrimitive::prim32 => ir::TyStd::Int32,
                    ir::TyPrimitive::prim64 => ir::TyStd::Int64,
                };
                self.duck_ty_std_name(ty).to_string()
            }

            ir::TyKind::Tuple(fields) if fields.is_empty() => {
                // Unit type
                "bool".to_string()
            }
            ir::TyKind::Tuple(fields) => {
                // Generate STRUCT(field1 type1, field2 type2, ...)
                let field_types: Vec<String> = fields
                    .iter()
                    .enumerate()
                    .map(|(p, f)| {
                        let name = utils::new_ident(super::repr_duckdb::field_name(f, p));
                        let f_type = self.duck_ty_name(&f.ty);
                        format!("{name} {f_type}")
                    })
                    .collect();
                format!("STRUCT({})", field_types.join(", "))
            }
            ir::TyKind::Array(item_ty) => {
                // Generate type[]
                let item_type = self.duck_ty_name(item_ty);
                format!("{item_type}[]")
            }
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                self.duck_ty_name(&variants[1].ty)
            }
            ir::TyKind::Enum(variants) => {
                // Generate UNION(variant1 type1, variant2 type2, ...)
                let variant_types: Vec<String> = variants
                    .iter()
                    .map(|v| {
                        let variant_ty = if v.ty.is_unit() {
                            // Unit variants use BOOL as placeholder (DuckDB UNION needs a type)
                            "BOOL".to_string()
                        } else {
                            self.duck_ty_name(&v.ty)
                        };
                        format!("{} {}", utils::new_ident(&v.name), variant_ty)
                    })
                    .collect();
                format!("UNION({})", variant_types.join(", "))
            }
            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    fn duck_ty_std_name(&self, ty: ir::TyStd) -> &'static str {
        use ir::TyStd::*;
        match ty {
            Bool => "BOOL",
            Int8 => "INT1",
            Int16 => "INT2",
            Int32 => "INT4",
            Int64 => "INT8",
            UInt8 => "UINT8",
            UInt16 => "UINT16",
            UInt32 => "UINT32",
            UInt64 => "UINT64",
            Float32 => "FLOAT4",
            Float64 => "FLOAT8",
            Text => "TEXT",
            Date => "DATE",
            Time => "TIMESTAMP",
            Timestamp => "TIMESTAMP",
            Decimal => "DECIMAL",
        }
    }

    /// Get default value for a type
    pub(super) fn default_value(&self, ty: &ir::Ty) -> &'static str {
        use ir::TyStd::*;

        let ty = self.get_ty_mat(ty);
        let ty_std = match &ty.kind {
            ir::TyKind::Ident(ident) => ir::TyStd::try_new(ident).unwrap(),
            ir::TyKind::Primitive(ir::TyPrimitive::prim8) => Int8,
            ir::TyKind::Primitive(ir::TyPrimitive::prim16) => Int16,
            ir::TyKind::Primitive(ir::TyPrimitive::prim32) => Int32,
            ir::TyKind::Primitive(ir::TyPrimitive::prim64) => Int64,
            _ => todo!(),
        };

        match self.dialect() {
            Dialect::Postgres => match ty_std {
                Bool => "FALSE",

                Int8 | Int16 | UInt8 | UInt16 => "0::int2",
                Int32 | UInt32 => "0::int4",
                Int64 | UInt64 => "0::int8",
                Float32 => "0.0::float4",
                Float64 => "0.0::float8",
                Text => "''",
                Date | Time | Timestamp | Decimal => {
                    todo!()
                }
            },
            Dialect::DuckDB => match ty_std {
                Bool => "FALSE",

                Int8 => "0::INT1",
                Int16 => "0::INT2",
                Int32 => "0::INT4",
                Int64 => "0::INT8",
                UInt8 => "0::UINT8",
                UInt16 => "0::UINT16",
                UInt32 => "0::UINT32",
                UInt64 => "0::UINT64",
                Float32 => "0.0::FLOAT4",
                Float64 => "0.0::FLOAT8",
                Text => "''",
                Date | Time | Timestamp | Decimal => {
                    todo!()
                }
            },
        }
    }
}
