use crate::sql::Dialect;
use crate::sql::queries::Context;
use crate::sql::utils::RelCols;
use lutra_bin::ir;

use super::utils;

impl<'p> Context<'p> {
    /// Get SQL type name for a Lutra type (in query repr)
    pub(super) fn ty_name(&self, ty: &ir::Ty) -> String {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(prim) => self.compile_primitive_ty_name(*prim),

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
                self.ser_compile_ty_name(ty)
            }

            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Get SQL type name in the "serialized repr"
    fn ser_compile_ty_name(&self, ty: &ir::Ty) -> String {
        match self.dialect() {
            Dialect::Postgres => {
                // PostgreSQL uses jsonb for complex types
                "jsonb".to_string()
            }
            Dialect::DuckDB => {
                // DuckDB serializes to STRUCT/UNION/LIST
                self.duck_compile_ty_name(ty)
            }
        }
    }

    /// Primitive type name
    fn compile_primitive_ty_name(&self, prim: ir::TyPrimitive) -> String {
        match self.dialect() {
            Dialect::Postgres => self.pg_compile_primitive_ty_name(prim),
            Dialect::DuckDB => self.duck_compile_primitive_ty_name(prim),
        }
    }

    /// PostgreSQL primitive type names
    /// Note: PostgreSQL has no native unsigned types, so we use same-sized signed types.
    /// Large unsigned values wrap around to negative (lutra-runner-postgres casts back).
    /// Note: PostgreSQL has no int1/tinyint, so int8 uses smallint (int2).
    fn pg_compile_primitive_ty_name(&self, prim: ir::TyPrimitive) -> String {
        match prim {
            ir::TyPrimitive::bool => "bool",
            ir::TyPrimitive::int8 => "int2", // PostgreSQL has no int1, use int2
            ir::TyPrimitive::int16 => "int2",
            ir::TyPrimitive::int32 => "int4",
            ir::TyPrimitive::int64 => "int8",
            ir::TyPrimitive::uint8 => "int2", // PostgreSQL has no int1, use int2
            ir::TyPrimitive::uint16 => "int2", // Same size, large values wrap to negative
            ir::TyPrimitive::uint32 => "int4", // Same size, large values wrap to negative
            ir::TyPrimitive::uint64 => "int8", // Same size, large values wrap to negative
            ir::TyPrimitive::float32 => "float4",
            ir::TyPrimitive::float64 => "float8",
            ir::TyPrimitive::text => "text",
        }
        .to_string()
    }

    /// DuckDB primitive type names
    fn duck_compile_primitive_ty_name(&self, prim: ir::TyPrimitive) -> String {
        match prim {
            ir::TyPrimitive::bool => "BOOL",
            ir::TyPrimitive::int8 => "INT1",
            ir::TyPrimitive::int16 => "INT2",
            ir::TyPrimitive::int32 => "INT4",
            ir::TyPrimitive::int64 => "INT8",
            ir::TyPrimitive::uint8 => "UINT8",
            ir::TyPrimitive::uint16 => "UINT16",
            ir::TyPrimitive::uint32 => "UINT32",
            ir::TyPrimitive::uint64 => "UINT64",
            ir::TyPrimitive::float32 => "FLOAT4",
            ir::TyPrimitive::float64 => "FLOAT8",
            ir::TyPrimitive::text => "TEXT",
        }
        .to_string()
    }

    /// Get type name for a Lutra type in "duckdb repr"
    pub(super) fn duck_compile_ty_name(&self, ty: &ir::Ty) -> String {
        // special case: std::Date
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && ty_ident.0 == ["std", "Date"]
        {
            return "DATE".into();
        }
        // special case: std::Time & std::Timestamp
        if let ir::TyKind::Ident(ty_ident) = &ty.kind
            && (ty_ident.0 == ["std", "Time"] || ty_ident.0 == ["std", "Timestamp"])
        {
            return "TIMESTAMP".into();
        }

        let ty_mat = self.get_ty_mat(ty);
        match &ty_mat.kind {
            ir::TyKind::Primitive(prim) => self.compile_primitive_ty_name(*prim),
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
                        let f_type = self.duck_compile_ty_name(&f.ty);
                        format!("{name} {f_type}")
                    })
                    .collect();
                format!("STRUCT({})", field_types.join(", "))
            }
            ir::TyKind::Array(item_ty) => {
                // Generate type[]
                let item_type = self.duck_compile_ty_name(item_ty);
                format!("{item_type}[]")
            }
            ir::TyKind::Enum(variants) if self.is_option(variants) => {
                self.duck_compile_ty_name(&variants[1].ty)
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
                            self.duck_compile_ty_name(&v.ty)
                        };
                        format!("{} {}", utils::new_ident(&v.name), variant_ty)
                    })
                    .collect();
                format!("UNION({})", variant_types.join(", "))
            }
            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        }
    }

    /// Get default value for a type
    pub(super) fn default_value(&self, ty: &ir::Ty) -> String {
        match self.dialect() {
            Dialect::Postgres => self.pg_default_value(ty),
            Dialect::DuckDB => self.duck_default_value(ty),
        }
    }

    fn pg_default_value(&self, ty: &ir::Ty) -> String {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(ir::TyPrimitive::int8) => "0::int2",
            ir::TyKind::Primitive(ir::TyPrimitive::int16) => "0::int2",
            ir::TyKind::Primitive(ir::TyPrimitive::int32) => "0::int4",
            ir::TyKind::Primitive(ir::TyPrimitive::int64) => "0::int8",
            ir::TyKind::Primitive(ir::TyPrimitive::float32) => "0.0::float4",
            ir::TyKind::Primitive(ir::TyPrimitive::float64) => "0.0::float8",
            ir::TyKind::Primitive(ir::TyPrimitive::bool) => "FALSE",
            ir::TyKind::Primitive(ir::TyPrimitive::text) => "''",
            _ => todo!(),
        }
        .to_string()
    }

    fn duck_default_value(&self, ty: &ir::Ty) -> String {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(ir::TyPrimitive::int8) => "0::INT1",
            ir::TyKind::Primitive(ir::TyPrimitive::int16) => "0::INT2",
            ir::TyKind::Primitive(ir::TyPrimitive::int32) => "0::INT4",
            ir::TyKind::Primitive(ir::TyPrimitive::int64) => "0::INT8",
            ir::TyKind::Primitive(ir::TyPrimitive::uint8) => "0::UINT8",
            ir::TyKind::Primitive(ir::TyPrimitive::uint16) => "0::UINT16",
            ir::TyKind::Primitive(ir::TyPrimitive::uint32) => "0::UINT32",
            ir::TyKind::Primitive(ir::TyPrimitive::uint64) => "0::UINT64",
            ir::TyKind::Primitive(ir::TyPrimitive::float32) => "0.0::FLOAT4",
            ir::TyKind::Primitive(ir::TyPrimitive::float64) => "0.0::FLOAT8",
            ir::TyKind::Primitive(ir::TyPrimitive::bool) => "FALSE",
            ir::TyKind::Primitive(ir::TyPrimitive::text) => "''",
            _ => todo!(),
        }
        .to_string()
    }
}
