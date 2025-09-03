#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec::Vec,
};

use core::fmt::{self, Display};

use crate::{SelectInto, Values};

use super::display_utils::{Indent, SpaceOrNewline, indented_list};
use super::{
    Expr, Ident, ObjectName, OrderByExpr, Query, SelectItem, TableWithJoins,
    display_comma_separated,
};

/// INSERT statement.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Insert {
    /// TABLE
    pub table: ObjectName,
    /// COLUMNS
    pub columns: Vec<Ident>,
    /// A SQL query that specifies what to insert
    pub source: Box<Query>,
}

impl Display for Insert {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "INSERT INTO {} ", self.table)?;

        if !self.columns.is_empty() {
            write!(f, "({})", display_comma_separated(&self.columns))?;
            SpaceOrNewline.fmt(f)?;
        }
        self.source.fmt(f)?;
        Ok(())
    }
}

/// DELETE statement.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Delete {
    /// Multi tables delete are supported in mysql
    pub tables: Vec<ObjectName>,
    /// FROM
    pub from: FromTable,
    /// USING (Snowflake, Postgres, MySQL)
    pub using: Option<Vec<TableWithJoins>>,
    /// WHERE
    pub selection: Option<Expr>,
    /// RETURNING
    pub returning: Option<Vec<SelectItem>>,
    /// ORDER BY (MySQL)
    pub order_by: Vec<OrderByExpr>,
    /// LIMIT (MySQL)
    pub limit: Option<Expr>,
}

impl Display for Delete {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("DELETE")?;
        if !self.tables.is_empty() {
            indented_list(f, &self.tables)?;
        }
        match &self.from {
            FromTable::WithFromKeyword(from) => {
                f.write_str(" FROM")?;
                indented_list(f, from)?;
            }
            FromTable::WithoutKeyword(from) => {
                indented_list(f, from)?;
            }
        }
        if let Some(using) = &self.using {
            SpaceOrNewline.fmt(f)?;
            f.write_str("USING")?;
            indented_list(f, using)?;
        }
        if let Some(selection) = &self.selection {
            SpaceOrNewline.fmt(f)?;
            f.write_str("WHERE")?;
            SpaceOrNewline.fmt(f)?;
            Indent(selection).fmt(f)?;
        }
        if let Some(returning) = &self.returning {
            SpaceOrNewline.fmt(f)?;
            f.write_str("RETURNING")?;
            indented_list(f, returning)?;
        }
        if !self.order_by.is_empty() {
            SpaceOrNewline.fmt(f)?;
            f.write_str("ORDER BY")?;
            indented_list(f, &self.order_by)?;
        }
        if let Some(limit) = &self.limit {
            SpaceOrNewline.fmt(f)?;
            f.write_str("LIMIT")?;
            SpaceOrNewline.fmt(f)?;
            Indent(limit).fmt(f)?;
        }
        Ok(())
    }
}
/// A `FROM` clause within a `DELETE` statement.
///
/// Syntax
/// ```sql
/// [FROM] table
/// ```
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum FromTable {
    /// An explicit `FROM` keyword was specified.
    WithFromKeyword(Vec<TableWithJoins>),
    /// BigQuery: `FROM` keyword was omitted.
    /// <https://cloud.google.com/bigquery/docs/reference/standard-sql/dml-syntax#delete_statement>
    WithoutKeyword(Vec<TableWithJoins>),
}
impl Display for FromTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FromTable::WithFromKeyword(tables) => {
                write!(f, "FROM {}", display_comma_separated(tables))
            }
            FromTable::WithoutKeyword(tables) => {
                write!(f, "{}", display_comma_separated(tables))
            }
        }
    }
}

/// SQL assignment `foo = expr` as used in SQLUpdate
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Assignment {
    pub target: AssignmentTarget,
    pub value: Expr,
}

impl fmt::Display for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} = {}", self.target, self.value)
    }
}

/// Left-hand side of an assignment in an UPDATE statement,
/// e.g. `foo` in `foo = 5` (ColumnName assignment) or
/// `(a, b)` in `(a, b) = (1, 2)` (Tuple assignment).
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum AssignmentTarget {
    /// A single column
    ColumnName(ObjectName),
    /// A tuple of columns
    Tuple(Vec<ObjectName>),
}

impl fmt::Display for AssignmentTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AssignmentTarget::ColumnName(column) => write!(f, "{column}"),
            AssignmentTarget::Tuple(columns) => write!(f, "({})", display_comma_separated(columns)),
        }
    }
}

/// Variant of `WHEN` clause used within a `MERGE` Statement.
///
/// Example:
/// ```sql
/// MERGE INTO T USING U ON FALSE WHEN MATCHED THEN DELETE
/// ```
/// [Snowflake](https://docs.snowflake.com/en/sql-reference/sql/merge)
/// [BigQuery](https://cloud.google.com/bigquery/docs/reference/standard-sql/dml-syntax#merge_statement)
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum MergeClauseKind {
    /// `WHEN MATCHED`
    Matched,
    /// `WHEN NOT MATCHED`
    NotMatched,
    /// `WHEN MATCHED BY TARGET`
    ///
    /// [BigQuery](https://cloud.google.com/bigquery/docs/reference/standard-sql/dml-syntax#merge_statement)
    NotMatchedByTarget,
    /// `WHEN MATCHED BY SOURCE`
    ///
    /// [BigQuery](https://cloud.google.com/bigquery/docs/reference/standard-sql/dml-syntax#merge_statement)
    NotMatchedBySource,
}

impl Display for MergeClauseKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MergeClauseKind::Matched => write!(f, "MATCHED"),
            MergeClauseKind::NotMatched => write!(f, "NOT MATCHED"),
            MergeClauseKind::NotMatchedByTarget => write!(f, "NOT MATCHED BY TARGET"),
            MergeClauseKind::NotMatchedBySource => write!(f, "NOT MATCHED BY SOURCE"),
        }
    }
}

/// The type of expression used to insert rows within a `MERGE` statement.
///
/// [Snowflake](https://docs.snowflake.com/en/sql-reference/sql/merge)
/// [BigQuery](https://cloud.google.com/bigquery/docs/reference/standard-sql/dml-syntax#merge_statement)
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum MergeInsertKind {
    /// The insert expression is defined from an explicit `VALUES` clause
    ///
    /// Example:
    /// ```sql
    /// INSERT VALUES(product, quantity)
    /// ```
    Values(Values),
    /// The insert expression is defined using only the `ROW` keyword.
    ///
    /// Example:
    /// ```sql
    /// INSERT ROW
    /// ```
    /// [BigQuery](https://cloud.google.com/bigquery/docs/reference/standard-sql/dml-syntax#merge_statement)
    Row,
}

impl Display for MergeInsertKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MergeInsertKind::Values(values) => {
                write!(f, "{values}")
            }
            MergeInsertKind::Row => {
                write!(f, "ROW")
            }
        }
    }
}

/// The expression used to insert rows within a `MERGE` statement.
///
/// Examples
/// ```sql
/// INSERT (product, quantity) VALUES(product, quantity)
/// INSERT ROW
/// ```
///
/// [Snowflake](https://docs.snowflake.com/en/sql-reference/sql/merge)
/// [BigQuery](https://cloud.google.com/bigquery/docs/reference/standard-sql/dml-syntax#merge_statement)
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct MergeInsertExpr {
    /// Columns (if any) specified by the insert.
    ///
    /// Example:
    /// ```sql
    /// INSERT (product, quantity) VALUES(product, quantity)
    /// INSERT (product, quantity) ROW
    /// ```
    pub columns: Vec<Ident>,
    /// The insert type used by the statement.
    pub kind: MergeInsertKind,
}

impl Display for MergeInsertExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.columns.is_empty() {
            write!(f, "({}) ", display_comma_separated(self.columns.as_slice()))?;
        }
        write!(f, "{}", self.kind)
    }
}

/// Underlying statement of a when clause within a `MERGE` Statement
///
/// Example
/// ```sql
/// INSERT (product, quantity) VALUES(product, quantity)
/// ```
///
/// [Snowflake](https://docs.snowflake.com/en/sql-reference/sql/merge)
/// [BigQuery](https://cloud.google.com/bigquery/docs/reference/standard-sql/dml-syntax#merge_statement)
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum MergeAction {
    /// An `INSERT` clause
    ///
    /// Example:
    /// ```sql
    /// INSERT (product, quantity) VALUES(product, quantity)
    /// ```
    Insert(MergeInsertExpr),
    /// An `UPDATE` clause
    ///
    /// Example:
    /// ```sql
    /// UPDATE SET quantity = T.quantity + S.quantity
    /// ```
    Update { assignments: Vec<Assignment> },
    /// A plain `DELETE` clause
    Delete,
}

impl Display for MergeAction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MergeAction::Insert(insert) => {
                write!(f, "INSERT {insert}")
            }
            MergeAction::Update { assignments } => {
                write!(f, "UPDATE SET {}", display_comma_separated(assignments))
            }
            MergeAction::Delete => {
                write!(f, "DELETE")
            }
        }
    }
}

/// A when clause within a `MERGE` Statement
///
/// Example:
/// ```sql
/// WHEN NOT MATCHED BY SOURCE AND product LIKE '%washer%' THEN DELETE
/// ```
/// [Snowflake](https://docs.snowflake.com/en/sql-reference/sql/merge)
/// [BigQuery](https://cloud.google.com/bigquery/docs/reference/standard-sql/dml-syntax#merge_statement)
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct MergeClause {
    pub clause_kind: MergeClauseKind,
    pub predicate: Option<Expr>,
    pub action: MergeAction,
}

impl Display for MergeClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MergeClause {
            clause_kind,
            predicate,
            action,
        } = self;

        write!(f, "WHEN {clause_kind}")?;
        if let Some(pred) = predicate {
            write!(f, " AND {pred}")?;
        }
        write!(f, " THEN {action}")
    }
}

/// A Output Clause in the end of a 'MERGE' Statement
///
/// Example:
/// OUTPUT $action, deleted.* INTO dbo.temp_products;
/// [mssql](https://learn.microsoft.com/en-us/sql/t-sql/queries/output-clause-transact-sql)
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum OutputClause {
    Output {
        select_items: Vec<SelectItem>,
        into_table: Option<SelectInto>,
    },
    Returning {
        select_items: Vec<SelectItem>,
    },
}

impl fmt::Display for OutputClause {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OutputClause::Output {
                select_items,
                into_table,
            } => {
                f.write_str("OUTPUT ")?;
                display_comma_separated(select_items).fmt(f)?;
                if let Some(into_table) = into_table {
                    f.write_str(" ")?;
                    into_table.fmt(f)?;
                }
                Ok(())
            }
            OutputClause::Returning { select_items } => {
                f.write_str("RETURNING ")?;
                display_comma_separated(select_items).fmt(f)
            }
        }
    }
}
