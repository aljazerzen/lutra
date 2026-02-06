use crate::sql::Dialect;
use crate::sql::queries::Context;
use lutra_bin::ir;

impl<'p> Context<'p> {
    /// Get SQL type name for a Lutra type, dispatching to dialect-specific implementations
    pub(super) fn compile_ty_name(&self, ty: &ir::Ty) -> String {
        match self.dialect() {
            Dialect::Postgres => self.pg_compile_ty_name(ty),
            Dialect::DuckDB => self.duck_compile_ty_name(ty),
        }
    }

    /// PostgreSQL type name generation
    fn pg_compile_ty_name(&self, ty: &ir::Ty) -> String {
        match &self.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(prim) => self.compile_primitive_ty_name(*prim),
            ir::TyKind::Tuple(fields) if fields.is_empty() => {
                // unit type (holds no data) does not exist in sql so we use a type with
                // the least amount of data
                "bool".to_string()
            }
            ir::TyKind::Enum(variants) if super::utils::is_maybe(variants) => {
                self.pg_compile_ty_name(&variants[1].ty)
            }
            // PostgreSQL uses jsonb for complex types
            ir::TyKind::Tuple(_) | ir::TyKind::Array(_) | ir::TyKind::Enum(_) => {
                "jsonb".to_string()
            }

            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }

    /// DuckDB type name generation (supports native LIST and STRUCT)
    pub(super) fn duck_compile_ty_name(&self, ty: &ir::Ty) -> String {
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
                    .map(|(position, field)| {
                        let field_name = field
                            .name
                            .as_deref()
                            .unwrap_or_else(|| Box::leak(format!("_{position}").into_boxed_str()));
                        let field_type = self.duck_compile_ty_name(&field.ty);
                        format!("{field_name} {field_type}")
                    })
                    .collect();
                format!("STRUCT({})", field_types.join(", "))
            }
            ir::TyKind::Array(item_ty) => {
                // Generate type[]
                let item_type = self.duck_compile_ty_name(item_ty);
                format!("{item_type}[]")
            }
            ir::TyKind::Enum(variants) if super::utils::is_maybe(variants) => {
                self.duck_compile_ty_name(&variants[1].ty)
            }
            ir::TyKind::Enum(_) => {
                // Enums use jsonb for now
                "jsonb".to_string()
            }
            ir::TyKind::Function(_) => todo!(),
            ir::TyKind::Ident(_) => todo!(),
        }
    }

    /// Primitive type name (same for all dialects)
    fn compile_primitive_ty_name(&self, prim: ir::TyPrimitive) -> String {
        match prim {
            ir::TyPrimitive::bool => "bool",
            ir::TyPrimitive::int8 => "int2",
            ir::TyPrimitive::uint8 => "int2",
            ir::TyPrimitive::int16 => "int2",
            ir::TyPrimitive::uint16 => "int2",
            ir::TyPrimitive::int32 => "int4",
            ir::TyPrimitive::uint32 => "int4",
            ir::TyPrimitive::int64 => "int8",
            ir::TyPrimitive::uint64 => "int8",
            ir::TyPrimitive::float32 => "float4",
            ir::TyPrimitive::float64 => "float8",
            ir::TyPrimitive::text => "text",
        }
        .to_string()
    }
}
