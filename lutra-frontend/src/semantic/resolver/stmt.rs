use crate::ir::decl::DeclKind;
use crate::ir::fold::PrFold;
use crate::pr;
use crate::semantic::NS_STD;
use crate::Result;

impl super::Resolver<'_> {
    /// Entry point to the resolver.
    /// fq_ident must point to an unresolved declaration.
    pub fn resolve_decl(&mut self, fq_ident: &pr::Path) -> Result<()> {
        if !fq_ident.starts_with_part(NS_STD) {
            log::debug!("resolving decl {fq_ident}");
        }

        self.debug_current_decl = fq_ident.clone();

        // take decl out of the module
        let decl = self.root_mod.module.get_mut(fq_ident).unwrap();

        let decl_kind = match &mut decl.kind {
            // happens in first pass
            DeclKind::Unresolved(stmt) => {
                let stmt = stmt.take().unwrap();
                self.resolve_unresolved(fq_ident, stmt)?
            }

            // happens in strict (second) pass
            DeclKind::Ty(ty) => {
                let ty_orig = ty.clone();

                DeclKind::Ty(self.fold_type(ty_orig)?)
            }

            DeclKind::Module(_) => unreachable!(),
            DeclKind::Expr(_) => unreachable!(),
            DeclKind::Import(_) => unreachable!(),
        };

        // put decl back in
        let decl = self.root_mod.module.get_mut(fq_ident).unwrap();
        decl.kind = decl_kind;
        Ok(())
    }

    pub fn resolve_unresolved(
        &mut self,
        fq_ident: &pr::Path,
        stmt: pr::StmtKind,
    ) -> Result<DeclKind> {
        // resolve
        Ok(match stmt {
            pr::StmtKind::ModuleDef(_) => {
                unreachable!("module def cannot be unresolved at this point")
                // it should have been converted into Module in resolve_decls::init_module_tree
            }
            pr::StmtKind::VarDef(var_def) => {
                let def = self.fold_var_def(var_def)?;
                let expected_ty = def.ty;

                match def.value {
                    Some(mut def_value) => {
                        // var value is provided

                        // validate type
                        if expected_ty.is_some() {
                            let who = || Some(fq_ident.name().to_string());
                            self.validate_expr_type(&mut def_value, expected_ty.as_ref(), &who)?;
                        }

                        // finalize global generics
                        // if let Some(mapping) = self.finalize_global_generics() {
                        //     let ty = def_value.ty.unwrap();
                        //     def_value.ty = Some(TypeReplacer::on_ty(ty, mapping));
                        // }

                        DeclKind::Expr(def_value)
                    }
                    None => {
                        // var value is not provided: treat this var as a param
                        let mut expr = Box::new(pr::Expr::new(pr::ExprKind::Param(
                            fq_ident.name().to_string(),
                        )));
                        expr.ty = expected_ty;
                        DeclKind::Expr(expr)
                    }
                }
            }
            pr::StmtKind::TypeDef(ty_def) => {
                let mut ty = self.fold_type(ty_def.ty)?;
                ty.name = Some(fq_ident.name().to_string());
                DeclKind::Ty(ty)
            }
            pr::StmtKind::ImportDef(target) => DeclKind::Import(target.name),
        })
    }
}
