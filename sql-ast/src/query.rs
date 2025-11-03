#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec::Vec};

use super::display_utils::{SpaceOrNewline, indented_list};
use crate::{
    dml::{Assignment, MergeClause, OutputClause},
    *,
};

/// The most complete variant of a `SELECT` query expression, optionally
/// including `WITH`, `UNION` / other set operations, and `ORDER BY`.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Query {
    /// WITH (common table expressions, or CTEs)
    pub with: Option<With>,
    /// SELECT or UNION / EXCEPT / INTERSECT
    pub body: Box<SetExpr>,
    /// ORDER BY
    pub order_by: Option<OrderBy>,
    /// LIMIT
    pub limit: Option<Expr>,
    /// OFFSET
    pub offset: Option<Expr>,
}

impl fmt::Display for Query {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref with) = self.with {
            with.fmt(f)?;
            SpaceOrNewline.fmt(f)?;
        }
        self.body.fmt(f)?;
        if let Some(ref order_by) = self.order_by {
            f.write_str(" ")?;
            order_by.fmt(f)?;
        }
        if let Some(ref offset) = self.offset {
            f.write_str(" OFFSET ")?;
            offset.fmt(f)?;
        }
        if let Some(ref limit) = self.limit {
            f.write_str(" LIMIT ")?;
            limit.fmt(f)?;
        }
        Ok(())
    }
}

/// A node in a tree, representing a "query body" expression, roughly:
/// `SELECT ... [ {UNION|EXCEPT|INTERSECT} SELECT ...]`
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum SetExpr {
    /// Restricted SELECT .. FROM .. HAVING (no ORDER BY or set operations)
    Select(Box<Select>),
    /// Parenthesized SELECT subquery, which may include more set operations
    /// in its body and an optional ORDER BY / LIMIT.
    Query(Box<Query>),
    /// UNION/EXCEPT/INTERSECT of two queries
    SetOperation {
        op: SetOperator,
        set_quantifier: SetQuantifier,
        left: Box<SetExpr>,
        right: Box<SetExpr>,
    },
    Values(Values),
    Insert(Insert),
    Update {
        /// TABLE
        table: RelNamed,
        /// Column assignments
        assignments: Vec<Assignment>,
        /// Table which provide value to be set
        from: Option<UpdateTableFromKind>,
        /// WHERE
        selection: Option<Expr>,
        /// RETURNING
        returning: Option<Vec<SelectItem>>,
        /// LIMIT
        limit: Option<Expr>,
    },
    Delete(Delete),
    Merge {
        /// optional INTO keyword
        into: bool,
        /// Specifies the table to merge
        table: RelNamed,
        /// Specifies the table or subquery to join with the target table
        source: RelNamed,
        /// Specifies the expression on which to join the target table and source
        on: Box<Expr>,
        /// Specifies the actions to perform when values match or do not match.
        clauses: Vec<MergeClause>,
        // Specifies the output to save changes in MSSQL
        output: Option<OutputClause>,
    },
    Source(String),
}

impl SetExpr {
    /// If this `SetExpr` is a `SELECT`, returns the [`Select`].
    pub fn as_select(&self) -> Option<&Select> {
        if let Self::Select(select) = self {
            Some(&**select)
        } else {
            None
        }
    }
}

impl fmt::Display for SetExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SetExpr::Select(s) => s.fmt(f),
            SetExpr::Query(q) => {
                f.write_str("(")?;
                q.fmt(f)?;
                f.write_str(")")
            }
            SetExpr::Values(v) => v.fmt(f),
            SetExpr::Insert(v) => v.fmt(f),
            SetExpr::Update {
                table,
                assignments,
                from,
                selection,
                returning,
                limit,
            } => {
                f.write_str("UPDATE ")?;
                table.fmt(f)?;
                if let Some(UpdateTableFromKind::BeforeSet(from)) = from {
                    SpaceOrNewline.fmt(f)?;
                    f.write_str("FROM")?;
                    indented_list(f, from)?;
                }
                if !assignments.is_empty() {
                    SpaceOrNewline.fmt(f)?;
                    f.write_str("SET")?;
                    indented_list(f, assignments)?;
                }
                if let Some(UpdateTableFromKind::AfterSet(from)) = from {
                    SpaceOrNewline.fmt(f)?;
                    f.write_str("FROM")?;
                    indented_list(f, from)?;
                }
                if let Some(selection) = selection {
                    SpaceOrNewline.fmt(f)?;
                    f.write_str("WHERE")?;
                    SpaceOrNewline.fmt(f)?;
                    Indent(selection).fmt(f)?;
                }
                if let Some(returning) = returning {
                    SpaceOrNewline.fmt(f)?;
                    f.write_str("RETURNING")?;
                    indented_list(f, returning)?;
                }
                if let Some(limit) = limit {
                    SpaceOrNewline.fmt(f)?;
                    write!(f, "LIMIT {limit}")?;
                }
                Ok(())
            }
            SetExpr::Delete(v) => v.fmt(f),
            SetExpr::Merge {
                into,
                table,
                source,
                on,
                clauses,
                output,
            } => {
                write!(
                    f,
                    "MERGE{int} {table} USING {source} ",
                    int = if *into { " INTO" } else { "" }
                )?;
                write!(f, "ON {on} ")?;
                write!(f, "{}", display_separated(clauses, " "))?;
                if let Some(output) = output {
                    write!(f, " {output}")?;
                }
                Ok(())
            }
            SetExpr::SetOperation {
                left,
                right,
                op,
                set_quantifier,
            } => {
                left.fmt(f)?;
                SpaceOrNewline.fmt(f)?;
                op.fmt(f)?;
                match set_quantifier {
                    SetQuantifier::All
                    | SetQuantifier::Distinct
                    | SetQuantifier::ByName
                    | SetQuantifier::AllByName
                    | SetQuantifier::DistinctByName => {
                        f.write_str(" ")?;
                        set_quantifier.fmt(f)?;
                    }
                    SetQuantifier::None => {}
                }
                SpaceOrNewline.fmt(f)?;
                right.fmt(f)?;
                Ok(())
            }
            SetExpr::Source(s) => f.write_str(s),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum SetOperator {
    Union,
    Except,
    Intersect,
    Minus,
}

impl fmt::Display for SetOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            SetOperator::Union => "UNION",
            SetOperator::Except => "EXCEPT",
            SetOperator::Intersect => "INTERSECT",
            SetOperator::Minus => "MINUS",
        })
    }
}

/// A quantifier for [SetOperator].
// TODO: Restrict parsing specific SetQuantifier in some specific dialects.
// For example, BigQuery does not support `DISTINCT` for `EXCEPT` and `INTERSECT`
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum SetQuantifier {
    All,
    Distinct,
    ByName,
    AllByName,
    DistinctByName,
    None,
}

impl fmt::Display for SetQuantifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SetQuantifier::All => write!(f, "ALL"),
            SetQuantifier::Distinct => write!(f, "DISTINCT"),
            SetQuantifier::ByName => write!(f, "BY NAME"),
            SetQuantifier::AllByName => write!(f, "ALL BY NAME"),
            SetQuantifier::DistinctByName => write!(f, "DISTINCT BY NAME"),
            SetQuantifier::None => Ok(()),
        }
    }
}

/// A restricted variant of `SELECT` (without CTEs/`ORDER BY`), which may
/// appear either as the only body item of a `Query`, or as an operand
/// to a set operation like `UNION`.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Select {
    /// `SELECT [DISTINCT] ...`
    pub distinct: Option<Distinct>,
    /// projection expressions
    pub projection: Vec<SelectItem>,
    /// INTO
    pub into: Option<SelectInto>,
    /// FROM
    pub from: Vec<RelNamed>,
    /// WHERE
    pub selection: Option<Expr>,
    /// GROUP BY
    pub group_by: GroupByExpr,
    /// SORT BY (Hive)
    pub sort_by: Vec<OrderByExpr>,
    /// HAVING
    pub having: Option<Expr>,
}

impl fmt::Display for Select {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SELECT")?;

        if let Some(ref distinct) = self.distinct {
            f.write_str(" ")?;
            distinct.fmt(f)?;
        }

        if !self.projection.is_empty() {
            indented_list(f, &self.projection)?;
        }

        if let Some(ref into) = self.into {
            f.write_str(" ")?;
            into.fmt(f)?;
        }

        if !self.from.is_empty() {
            SpaceOrNewline.fmt(f)?;
            f.write_str("FROM")?;
            indented_list(f, &self.from)?;
        }
        if let Some(ref selection) = self.selection {
            SpaceOrNewline.fmt(f)?;
            f.write_str("WHERE")?;
            SpaceOrNewline.fmt(f)?;
            Indent(selection).fmt(f)?;
        }
        match &self.group_by {
            GroupByExpr::All(_) => {
                SpaceOrNewline.fmt(f)?;
                self.group_by.fmt(f)?;
            }
            GroupByExpr::Expressions(exprs, _) => {
                if !exprs.is_empty() {
                    SpaceOrNewline.fmt(f)?;
                    self.group_by.fmt(f)?;
                }
            }
        }
        if !self.sort_by.is_empty() {
            SpaceOrNewline.fmt(f)?;
            f.write_str("SORT BY")?;
            SpaceOrNewline.fmt(f)?;
            Indent(display_comma_separated(&self.sort_by)).fmt(f)?;
        }
        if let Some(ref having) = self.having {
            SpaceOrNewline.fmt(f)?;
            f.write_str("HAVING")?;
            SpaceOrNewline.fmt(f)?;
            Indent(having).fmt(f)?;
        }
        Ok(())
    }
}

/// A hive LATERAL VIEW with potential column aliases
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct LateralView {
    /// LATERAL VIEW
    pub lateral_view: Expr,
    /// LATERAL VIEW table name
    pub lateral_view_name: ObjectName,
    /// LATERAL VIEW optional column aliases
    pub lateral_col_alias: Vec<Ident>,
    /// LATERAL VIEW OUTER
    pub outer: bool,
}

impl fmt::Display for LateralView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            " LATERAL VIEW{outer} {} {}",
            self.lateral_view,
            self.lateral_view_name,
            outer = if self.outer { " OUTER" } else { "" }
        )?;
        if !self.lateral_col_alias.is_empty() {
            write!(
                f,
                " AS {}",
                display_comma_separated(&self.lateral_col_alias)
            )?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct With {
    /// Token for the "WITH" keyword
    pub recursive: bool,
    pub cte_tables: Vec<Cte>,
}

impl fmt::Display for With {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("WITH ")?;
        if self.recursive {
            f.write_str("RECURSIVE ")?;
        }
        display_comma_separated(&self.cte_tables).fmt(f)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum CteAsMaterialized {
    /// The `WITH` statement specifies `AS MATERIALIZED` behavior
    Materialized,
    /// The `WITH` statement specifies `AS NOT MATERIALIZED` behavior
    NotMaterialized,
}

impl fmt::Display for CteAsMaterialized {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CteAsMaterialized::Materialized => {
                write!(f, "MATERIALIZED")?;
            }
            CteAsMaterialized::NotMaterialized => {
                write!(f, "NOT MATERIALIZED")?;
            }
        };
        Ok(())
    }
}

/// A single CTE (used after `WITH`): `<alias> [(col1, col2, ...)] AS <materialized> ( <query> )`
/// The names in the column list before `AS`, when specified, replace the names
/// of the columns returned by the query. The parser does not validate that the
/// number of columns in the query matches the number of columns in the query.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Cte {
    pub alias: TableAlias,
    pub query: Box<Query>,
    pub materialized: Option<CteAsMaterialized>,
}

impl fmt::Display for Cte {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.materialized.as_ref() {
            None => {
                self.alias.fmt(f)?;
                f.write_str(" AS (")?;
                NewLine.fmt(f)?;
                Indent(&self.query).fmt(f)?;
                NewLine.fmt(f)?;
                f.write_str(")")?;
            }
            Some(materialized) => {
                self.alias.fmt(f)?;
                f.write_str(" AS ")?;
                materialized.fmt(f)?;
                f.write_str(" (")?;
                NewLine.fmt(f)?;
                Indent(&self.query).fmt(f)?;
                NewLine.fmt(f)?;
                f.write_str(")")?;
            }
        };
        Ok(())
    }
}

/// One item of the comma-separated list following `SELECT`
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct SelectItem {
    pub expr: Expr,
    pub alias: Option<Ident>,
}

impl fmt::Display for SelectItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.expr.fmt(f)?;
        if let Some(alias) = &self.alias {
            f.write_str(" AS ")?;
            alias.fmt(f)?;
        }
        Ok(())
    }
}

/// An expression optionally followed by an alias.
///
/// Example:
/// ```sql
/// 42 AS myint
/// ```
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ExprWithAlias {
    pub expr: Expr,
    pub alias: Option<Ident>,
}

impl fmt::Display for ExprWithAlias {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ExprWithAlias { expr, alias } = self;
        write!(f, "{expr}")?;
        if let Some(alias) = alias {
            write!(f, " AS {alias}")?;
        }
        Ok(())
    }
}

/// A table name or a parenthesized subquery with an optional alias
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct RelNamed {
    pub lateral: bool,
    pub alias: Option<TableAlias>,
    pub expr: RelExpr,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum RelExpr {
    Table {
        name: ObjectName,
    },
    Subquery {
        subquery: Box<Query>,
    },
    Function {
        name: ObjectName,
        args: Vec<Expr>,
    },
    /// ```sql
    /// SELECT * FROM UNNEST ([10,20,30]) as numbers WITH OFFSET;
    /// +---------+--------+
    /// | numbers | offset |
    /// +---------+--------+
    /// | 10      | 0      |
    /// | 20      | 1      |
    /// | 30      | 2      |
    /// +---------+--------+
    /// ```
    UNNEST {
        array_exprs: Vec<Expr>,
        with_offset: bool,
        with_offset_alias: Option<Ident>,
        with_ordinality: bool,
    },

    /// Represents PIVOT operation on a table.
    /// For example `FROM monthly_sales PIVOT(sum(amount) FOR MONTH IN ('JAN', 'FEB'))`
    ///
    /// [BigQuery](https://cloud.google.com/bigquery/docs/reference/standard-sql/query-syntax#pivot_operator)
    /// [Snowflake](https://docs.snowflake.com/en/sql-reference/constructs/pivot)
    Pivot {
        table: Box<RelNamed>,
        aggregate_functions: Vec<ExprWithAlias>, // Function expression
        value_column: Vec<Expr>,
        value_source: PivotValueSource,
        default_on_null: Option<Expr>,
    },
    /// An UNPIVOT operation on a table.
    ///
    /// Syntax:
    /// ```sql
    /// table UNPIVOT [ { INCLUDE | EXCLUDE } NULLS ] (value FOR name IN (column1, [ column2, ... ])) [ alias ]
    /// ```
    ///
    /// See <https://docs.snowflake.com/en/sql-reference/constructs/unpivot>.
    /// See <https://docs.databricks.com/aws/en/sql/language-manual/sql-ref-syntax-qry-select-unpivot>.
    Unpivot {
        table: Box<RelNamed>,
        value: Expr,
        name: Ident,
        columns: Vec<ExprWithAlias>,
    },
}

/// The source of values in a `PIVOT` operation.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum PivotValueSource {
    /// Pivot on a static list of values.
    ///
    /// See <https://docs.snowflake.com/en/sql-reference/constructs/pivot#pivot-on-a-specified-list-of-column-values-for-the-pivot-column>.
    List(Vec<ExprWithAlias>),
    /// Pivot on all distinct values of the pivot column.
    ///
    /// See <https://docs.snowflake.com/en/sql-reference/constructs/pivot#pivot-on-all-distinct-column-values-automatically-with-dynamic-pivot>.
    Any(Vec<OrderByExpr>),
    /// Pivot on all values returned by a subquery.
    ///
    /// See <https://docs.snowflake.com/en/sql-reference/constructs/pivot#pivot-on-column-values-using-a-subquery-with-dynamic-pivot>.
    Subquery(Box<Query>),
}

impl fmt::Display for PivotValueSource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PivotValueSource::List(values) => write!(f, "{}", display_comma_separated(values)),
            PivotValueSource::Any(order_by) => {
                write!(f, "ANY")?;
                if !order_by.is_empty() {
                    write!(f, " ORDER BY {}", display_comma_separated(order_by))?;
                }
                Ok(())
            }
            PivotValueSource::Subquery(query) => write!(f, "{query}"),
        }
    }
}

impl fmt::Display for RelNamed {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.lateral {
            write!(f, "LATERAL ")?;
        }
        match &self.expr {
            RelExpr::Table { name } => {
                name.fmt(f)?;
                if let Some(alias) = &self.alias {
                    write!(f, " AS {alias}")?;
                }
                Ok(())
            }
            RelExpr::Subquery { subquery } => {
                f.write_str("(")?;
                NewLine.fmt(f)?;
                Indent(subquery).fmt(f)?;
                NewLine.fmt(f)?;
                f.write_str(")")?;
                if let Some(alias) = &self.alias {
                    write!(f, " AS {alias}")?;
                }
                Ok(())
            }
            RelExpr::Function { name, args } => {
                write!(f, "{name}({})", display_comma_separated(args))?;
                if let Some(alias) = &self.alias {
                    write!(f, " AS {alias}")?;
                }
                Ok(())
            }
            RelExpr::UNNEST {
                array_exprs,
                with_offset,
                with_offset_alias,
                with_ordinality,
            } => {
                write!(f, "UNNEST({})", display_comma_separated(array_exprs))?;

                if *with_ordinality {
                    write!(f, " WITH ORDINALITY")?;
                }

                if let Some(alias) = &self.alias {
                    write!(f, " AS {alias}")?;
                }
                if *with_offset {
                    write!(f, " WITH OFFSET")?;
                }
                if let Some(alias) = with_offset_alias {
                    write!(f, " AS {alias}")?;
                }
                Ok(())
            }
            RelExpr::Pivot {
                table,
                aggregate_functions,
                value_column,
                value_source,
                default_on_null,
            } => {
                write!(
                    f,
                    "{table} PIVOT({} FOR ",
                    display_comma_separated(aggregate_functions),
                )?;
                if value_column.len() == 1 {
                    write!(f, "{}", value_column[0])?;
                } else {
                    write!(f, "({})", display_comma_separated(value_column))?;
                }
                write!(f, " IN ({value_source})")?;
                if let Some(expr) = default_on_null {
                    write!(f, " DEFAULT ON NULL ({expr})")?;
                }
                write!(f, ")")?;
                if let Some(alias) = &self.alias {
                    write!(f, " AS {alias}")?;
                }
                Ok(())
            }
            RelExpr::Unpivot {
                table,
                value,
                name,
                columns,
            } => {
                write!(f, "{table} UNPIVOT")?;
                write!(
                    f,
                    "({} FOR {} IN ({}))",
                    value,
                    name,
                    display_comma_separated(columns)
                )?;
                if let Some(alias) = &self.alias {
                    write!(f, " AS {alias}")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TableAlias {
    pub name: Ident,
    pub columns: Vec<Ident>,
}

impl fmt::Display for TableAlias {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.columns.is_empty() {
            write!(f, " ({})", display_comma_separated(&self.columns))?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum TableVersion {
    /// When the table version is defined using `FOR SYSTEM_TIME AS OF`.
    /// For example: `SELECT * FROM tbl FOR SYSTEM_TIME AS OF TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 1 HOUR)`
    ForSystemTimeAsOf(Expr),
    /// When the table version is defined using a function.
    /// For example: `SELECT * FROM tbl AT(TIMESTAMP => '2020-08-14 09:30:00')`
    Function(Expr),
}

impl Display for TableVersion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TableVersion::ForSystemTimeAsOf(e) => write!(f, "FOR SYSTEM_TIME AS OF {e}")?,
            TableVersion::Function(func) => write!(f, "{func}")?,
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Join {
    pub relation: RelNamed,
    pub join_operator: JoinOperator,
}

impl fmt::Display for Join {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn prefix(constraint: &JoinConstraint) -> &'static str {
            match constraint {
                JoinConstraint::Natural => "NATURAL ",
                _ => "",
            }
        }
        fn suffix(constraint: &'_ JoinConstraint) -> impl fmt::Display + '_ {
            struct Suffix<'a>(&'a JoinConstraint);
            impl fmt::Display for Suffix<'_> {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    match self.0 {
                        JoinConstraint::On(expr) => write!(f, " ON {expr}"),
                        JoinConstraint::Using(attrs) => {
                            write!(f, " USING({})", display_comma_separated(attrs))
                        }
                        _ => Ok(()),
                    }
                }
            }
            Suffix(constraint)
        }

        match &self.join_operator {
            JoinOperator::Join(constraint) => f.write_fmt(format_args!(
                "{}JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::Inner(constraint) => f.write_fmt(format_args!(
                "{}INNER JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::Left(constraint) => f.write_fmt(format_args!(
                "{}LEFT JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::LeftOuter(constraint) => f.write_fmt(format_args!(
                "{}LEFT OUTER JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::Right(constraint) => f.write_fmt(format_args!(
                "{}RIGHT JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::RightOuter(constraint) => f.write_fmt(format_args!(
                "{}RIGHT OUTER JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::FullOuter(constraint) => f.write_fmt(format_args!(
                "{}FULL JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::CrossJoin => f.write_fmt(format_args!("CROSS JOIN {}", self.relation)),
            JoinOperator::Semi(constraint) => f.write_fmt(format_args!(
                "{}SEMI JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::LeftSemi(constraint) => f.write_fmt(format_args!(
                "{}LEFT SEMI JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::RightSemi(constraint) => f.write_fmt(format_args!(
                "{}RIGHT SEMI JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::Anti(constraint) => f.write_fmt(format_args!(
                "{}ANTI JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::LeftAnti(constraint) => f.write_fmt(format_args!(
                "{}LEFT ANTI JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::RightAnti(constraint) => f.write_fmt(format_args!(
                "{}RIGHT ANTI JOIN {}{}",
                prefix(constraint),
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::CrossApply => f.write_fmt(format_args!("CROSS APPLY {}", self.relation)),
            JoinOperator::OuterApply => f.write_fmt(format_args!("OUTER APPLY {}", self.relation)),
            JoinOperator::AsOf {
                match_condition,
                constraint,
            } => f.write_fmt(format_args!(
                "ASOF JOIN {} MATCH_CONDITION ({match_condition}){}",
                self.relation,
                suffix(constraint)
            )),
            JoinOperator::StraightJoin(constraint) => f.write_fmt(format_args!(
                "STRAIGHT_JOIN {}{}",
                self.relation,
                suffix(constraint)
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum JoinOperator {
    Join(JoinConstraint),
    Inner(JoinConstraint),
    Left(JoinConstraint),
    LeftOuter(JoinConstraint),
    Right(JoinConstraint),
    RightOuter(JoinConstraint),
    FullOuter(JoinConstraint),
    CrossJoin,
    /// SEMI (non-standard)
    Semi(JoinConstraint),
    /// LEFT SEMI (non-standard)
    LeftSemi(JoinConstraint),
    /// RIGHT SEMI (non-standard)
    RightSemi(JoinConstraint),
    /// ANTI (non-standard)
    Anti(JoinConstraint),
    /// LEFT ANTI (non-standard)
    LeftAnti(JoinConstraint),
    /// RIGHT ANTI (non-standard)
    RightAnti(JoinConstraint),
    /// CROSS APPLY (non-standard)
    CrossApply,
    /// OUTER APPLY (non-standard)
    OuterApply,
    /// `ASOF` joins are used for joining tables containing time-series data
    /// whose timestamp columns do not match exactly.
    ///
    /// See <https://docs.snowflake.com/en/sql-reference/constructs/asof-join>.
    AsOf {
        match_condition: Expr,
        constraint: JoinConstraint,
    },
    /// STRAIGHT_JOIN (non-standard)
    ///
    /// See <https://dev.mysql.com/doc/refman/8.4/en/join.html>.
    StraightJoin(JoinConstraint),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum JoinConstraint {
    On(Expr),
    Using(Vec<ObjectName>),
    Natural,
    None,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum OrderByKind {
    /// ALL syntax of [DuckDB] and [ClickHouse].
    ///
    /// [DuckDB]:  <https://duckdb.org/docs/sql/query_syntax/orderby>
    /// [ClickHouse]: <https://clickhouse.com/docs/en/sql-reference/statements/select/order-by>
    All(OrderByOptions),

    /// Expressions
    Expressions(Vec<OrderByExpr>),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct OrderBy {
    pub exprs: Vec<OrderByExpr>,
}

impl fmt::Display for OrderBy {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ORDER BY")?;
        write!(f, " {}", display_comma_separated(&self.exprs))?;

        Ok(())
    }
}

/// An `ORDER BY` expression
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct OrderByExpr {
    pub expr: Expr,
    pub options: OrderByOptions,
}

impl fmt::Display for OrderByExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.expr, self.options)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct OrderByOptions {
    /// Optional `ASC` or `DESC`
    pub asc: Option<bool>,
    /// Optional `NULLS FIRST` or `NULLS LAST`
    pub nulls_first: Option<bool>,
}

impl fmt::Display for OrderByOptions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.asc {
            Some(true) => write!(f, " ASC")?,
            Some(false) => write!(f, " DESC")?,
            None => (),
        }
        match self.nulls_first {
            Some(true) => write!(f, " NULLS FIRST")?,
            Some(false) => write!(f, " NULLS LAST")?,
            None => (),
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Distinct {
    /// DISTINCT
    Distinct,

    /// DISTINCT ON({column names})
    On(Vec<Expr>),
}

impl fmt::Display for Distinct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Distinct::Distinct => write!(f, "DISTINCT"),
            Distinct::On(col_names) => {
                let col_names = display_comma_separated(col_names);
                write!(f, "DISTINCT ON ({col_names})")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Values {
    /// Was there an explicit ROWs keyword (MySQL)?
    /// <https://dev.mysql.com/doc/refman/8.0/en/values.html>
    pub explicit_row: bool,
    pub rows: Vec<Vec<Expr>>,
}

impl fmt::Display for Values {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("VALUES")?;
        let prefix = if self.explicit_row { "ROW" } else { "" };
        let mut delim = "";
        for row in &self.rows {
            f.write_str(delim)?;
            delim = ",";
            SpaceOrNewline.fmt(f)?;
            Indent(format_args!("{prefix}({})", display_comma_separated(row))).fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct SelectInto {
    pub temporary: bool,
    pub unlogged: bool,
    pub table: bool,
    pub name: ObjectName,
}

impl fmt::Display for SelectInto {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let temporary = if self.temporary { " TEMPORARY" } else { "" };
        let unlogged = if self.unlogged { " UNLOGGED" } else { "" };
        let table = if self.table { " TABLE" } else { "" };

        write!(f, "INTO{}{}{} {}", temporary, unlogged, table, self.name)
    }
}

/// ClickHouse supports GROUP BY WITH modifiers(includes ROLLUP|CUBE|TOTALS).
/// e.g. GROUP BY year WITH ROLLUP WITH TOTALS
///
/// [ClickHouse]: <https://clickhouse.com/docs/en/sql-reference/statements/select/group-by#rollup-modifier>
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum GroupByWithModifier {
    Rollup,
    Cube,
    Totals,
    /// Hive supports GROUP BY GROUPING SETS syntax.
    /// e.g. GROUP BY year , month GROUPING SETS((year,month),(year),(month))
    ///
    /// [Hive]: <https://cwiki.apache.org/confluence/pages/viewpage.action?pageId=30151323#EnhancedAggregation,Cube,GroupingandRollup-GROUPINGSETSclause>
    GroupingSets(Expr),
}

impl fmt::Display for GroupByWithModifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GroupByWithModifier::Rollup => write!(f, "WITH ROLLUP"),
            GroupByWithModifier::Cube => write!(f, "WITH CUBE"),
            GroupByWithModifier::Totals => write!(f, "WITH TOTALS"),
            GroupByWithModifier::GroupingSets(expr) => {
                write!(f, "{expr}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum GroupByExpr {
    /// ALL syntax of [Snowflake], [DuckDB] and [ClickHouse].
    ///
    /// [Snowflake]: <https://docs.snowflake.com/en/sql-reference/constructs/group-by#label-group-by-all-columns>
    /// [DuckDB]:  <https://duckdb.org/docs/sql/query_syntax/groupby.html>
    /// [ClickHouse]: <https://clickhouse.com/docs/en/sql-reference/statements/select/group-by#group-by-all>
    ///
    /// ClickHouse also supports WITH modifiers after GROUP BY ALL and expressions.
    ///
    /// [ClickHouse]: <https://clickhouse.com/docs/en/sql-reference/statements/select/group-by#rollup-modifier>
    All(Vec<GroupByWithModifier>),

    /// Expressions
    Expressions(Vec<Expr>, Vec<GroupByWithModifier>),
}

impl fmt::Display for GroupByExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GroupByExpr::All(modifiers) => {
                write!(f, "GROUP BY ALL")?;
                if !modifiers.is_empty() {
                    write!(f, " {}", display_separated(modifiers, " "))?;
                }
                Ok(())
            }
            GroupByExpr::Expressions(col_names, modifiers) => {
                f.write_str("GROUP BY")?;
                SpaceOrNewline.fmt(f)?;
                Indent(display_comma_separated(col_names)).fmt(f)?;
                if !modifiers.is_empty() {
                    write!(f, " {}", display_separated(modifiers, " "))?;
                }
                Ok(())
            }
        }
    }
}

/// The `FROM` clause of an `UPDATE TABLE` statement
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum UpdateTableFromKind {
    /// Update Statement where the 'FROM' clause is before the 'SET' keyword (Supported by Snowflake)
    /// For Example: `UPDATE FROM t1 SET t1.name='aaa'`
    BeforeSet(Vec<RelNamed>),
    /// Update Statement where the 'FROM' clause is after the 'SET' keyword (Which is the standard way)
    /// For Example: `UPDATE SET t1.name='aaa' FROM t1`
    AfterSet(Vec<RelNamed>),
}
