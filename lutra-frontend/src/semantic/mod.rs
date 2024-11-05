//! Semantic resolver (name resolution, type checking and lowering to RQ)

pub mod desugar;
mod module;
mod resolve_decls;
mod resolver;

use self::resolver::Resolver;

use crate::compile::is_mod_def_for;
use crate::ir::decl::RootModule;
use crate::pr;
use crate::Result;

/// Runs semantic analysis on the query.
pub fn resolve(module_tree: pr::ModuleDef) -> Result<RootModule> {
    // load_std_lib(&mut module_tree);

    // expand AST into PL
    let root_module_def = desugar::run(module_tree);

    // init the module structure
    let mut root_module = resolve_decls::init_module_tree(root_module_def);

    // resolve name references between declarations
    let resolution_order = resolve_decls::resolve_decl_refs(&mut root_module)?;

    // resolve
    let mut resolver = Resolver::new(&mut root_module);

    for decl_fq in &resolution_order {
        resolver.resolve_decl(decl_fq)?;
    }

    root_module.ordering = resolution_order;
    Ok(root_module)
}

/// Preferred way of injecting std module.
#[allow(dead_code)]
pub fn load_std_lib(module_tree: &mut pr::ModuleDef) {
    if !module_tree.stmts.iter().any(|s| is_mod_def_for(s, NS_STD)) {
        log::debug!("loading std.prql");

        let std_source = include_str!("std.prql");
        let (ast, errs) = lutra_parser::parse_source(std_source, 0);
        if let Some(stmts) = ast {
            let stmt = pr::Stmt::new(pr::StmtKind::ModuleDef(pr::ModuleDef {
                name: "std".to_string(),
                stmts,
            }));
            module_tree.stmts.insert(0, stmt);
        } else {
            panic!("std.prql failed to compile:\n{errs:?}");
        }
    }
}

pub fn is_ident_or_func_call(expr: &pr::Expr, name: &pr::Path) -> bool {
    match &expr.kind {
        pr::ExprKind::Ident(i) if i == name => true,
        pr::ExprKind::FuncCall(pr::FuncCall { name: n_expr, .. })
            if n_expr.kind.as_ident().map_or(false, |i| i == name) =>
        {
            true
        }
        _ => false,
    }
}

pub const NS_STD: &str = "std";
pub const NS_THIS: &str = "this";
pub const NS_MAIN: &str = "main";
pub const NS_LOCAL: &str = "_local";

#[cfg(test)]
pub mod test {
    use insta::assert_yaml_snapshot;

    use crate::ir::rq::RelationalQuery;
    use crate::parser::parse;
    use crate::Errors;

    use super::{resolve, resolve_and_lower, RootModule};

    pub fn parse_resolve_and_lower(query: &str) -> Result<RelationalQuery, Errors> {
        let source_tree = query.into();
        Ok(resolve_and_lower(parse(&source_tree)?, &[], None)?)
    }

    pub fn parse_and_resolve(query: &str) -> Result<RootModule, Errors> {
        let source_tree = query.into();
        Ok(resolve(parse(&source_tree)?)?)
    }

    #[test]
    fn test_resolve_01() {
        assert_yaml_snapshot!(parse_resolve_and_lower(r###"
        from db.employees
        select !{foo}
        "###).unwrap().relation.columns, @r###"
        ---
        - Wildcard
        "###)
    }

    #[test]
    fn test_resolve_02() {
        assert_yaml_snapshot!(parse_resolve_and_lower(r###"
        from db.foo
        sort day
        window range:-4..4 (
            derive {next_four_days = sum b}
        )
        "###).unwrap().relation.columns, @r###"
        ---
        - Single: day
        - Single: b
        - Wildcard
        - Single: next_four_days
        "###)
    }

    #[test]
    fn test_resolve_03() {
        assert_yaml_snapshot!(parse_resolve_and_lower(r###"
        from db.albums
        select {a = this}
        filter is_sponsored
        select {a.*}
        "###).unwrap().relation.columns, @r###"
        ---
        - Single: is_sponsored
        - Wildcard
        "###)
    }

    #[test]
    fn test_resolve_04() {
        assert_yaml_snapshot!(parse_resolve_and_lower(r###"
        from db.x
        select {a, a, a = a + 1}
        "###).unwrap().relation.columns, @r###"
        ---
        - Single: ~
        - Single: ~
        - Single: a
        "###)
    }

    #[test]
    fn test_header() {
        assert_yaml_snapshot!(parse_resolve_and_lower(r#"
        prql target:sql.mssql version:"0"

        from db.employees
        "#).unwrap(), @r###"
        ---
        def:
          version: ^0
          other:
            target: sql.mssql
        tables:
          - id: 0
            name: ~
            relation:
              kind:
                ExternRef:
                  LocalTable:
                    - employees
              columns:
                - Wildcard
        relation:
          kind:
            Pipeline:
              - From:
                  source: 0
                  columns:
                    - - Wildcard
                      - 0
                  name: employees
              - Select:
                  - 0
          columns:
            - Wildcard
        "### );

        assert!(parse_resolve_and_lower(
            r###"
        prql target:sql.bigquery version:foo
        from db.employees
        "###,
        )
        .is_err());

        assert!(parse_resolve_and_lower(
            r#"
        prql target:sql.bigquery version:"25"
        from db.employees
        "#,
        )
        .is_err());

        assert!(parse_resolve_and_lower(
            r###"
        prql target:sql.yah version:foo
        from db.employees
        "###,
        )
        .is_err());
    }
}
