//! SQL Abstract Syntax Tree (AST) types
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(not(feature = "std"))]
extern crate alloc;

mod display_utils;
mod dml;
mod query;
mod string;

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec::Vec,
};

use core::cmp::Ordering;
use core::{
    fmt::{self, Display},
    hash,
};

use display_utils::{NewLine, SpaceOrNewline};

pub use self::dml::{Assignment, AssignmentTarget, Delete, Insert, Update};
pub use self::query::{
    Copy, Cte, CteAsMaterialized, Distinct, ExprWithAlias, Join, JoinConstraint, JoinOperator,
    LateralView, OrderBy, OrderByExpr, OrderByKind, OrderByOptions, PivotValueSource, Query,
    RelExpr, RelNamed, Select, SelectInto, SelectItem, SetExpr, SetOperator, SetQuantifier,
    TableAlias, TableVersion, Values, With,
};

pub use self::string::escape as escape_string;
pub use display_utils::{DisplayCommaSeparated, Indent};

pub struct DisplaySeparated<'a, T>
where
    T: fmt::Display,
{
    slice: &'a [T],
    sep: &'static str,
}

impl<T> fmt::Display for DisplaySeparated<'_, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut delim = "";
        for t in self.slice {
            f.write_str(delim)?;
            delim = self.sep;
            t.fmt(f)?;
        }
        Ok(())
    }
}

pub fn display_separated<'a, T>(slice: &'a [T], sep: &'static str) -> DisplaySeparated<'a, T>
where
    T: fmt::Display,
{
    DisplaySeparated { slice, sep }
}

pub fn display_comma_separated<T>(slice: &[T]) -> DisplaySeparated<'_, T>
where
    T: fmt::Display,
{
    DisplaySeparated { slice, sep: ", " }
}

/// An identifier, decomposed into its value or character data and the quote style.
#[derive(Debug, Clone)]
pub struct Ident {
    /// The value of the identifier without quotes.
    pub value: String,
    /// The starting quote if any. Valid quote characters are the single quote,
    /// double quote, backtick, and opening square bracket.
    pub quote_style: Option<char>,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        let Ident {
            value,
            quote_style,
            // exhaustiveness check; we ignore spans in comparisons
        } = self;

        value == &other.value && quote_style == &other.quote_style
    }
}

impl core::hash::Hash for Ident {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        let Ident {
            value,
            quote_style,
            // exhaustiveness check; we ignore spans in hashes
        } = self;

        value.hash(state);
        quote_style.hash(state);
    }
}

impl Eq for Ident {}

impl PartialOrd for Ident {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Ident {
    fn cmp(&self, other: &Self) -> Ordering {
        let Ident {
            value,
            quote_style,
            // exhaustiveness check; we ignore spans in ordering
        } = self;

        let Ident {
            value: other_value,
            quote_style: other_quote_style,
            // exhaustiveness check; we ignore spans in ordering
        } = other;

        // First compare by value, then by quote_style
        value
            .cmp(other_value)
            .then_with(|| quote_style.cmp(other_quote_style))
    }
}

impl Ident {
    /// Create a new identifier with the given value and no quotes and an empty span.
    pub fn new<S>(value: S) -> Self
    where
        S: Into<String>,
    {
        Ident {
            value: value.into(),
            quote_style: None,
        }
    }

    /// Create a new quoted identifier with the given quote and value. This function
    /// panics if the given quote is not a valid quote character.
    pub fn with_quote_if_needed<S>(quote: char, value: S) -> Self
    where
        S: Into<String>,
    {
        let value = value.into();
        let quote_style = if valid_ident_regex().is_match(&value) && !is_keyword(&value) {
            None
        } else {
            Some(quote)
        };
        Ident { value, quote_style }
    }
}

fn valid_ident_regex() -> &'static regex::Regex {
    static VALID_IDENT: std::sync::OnceLock<regex::Regex> = std::sync::OnceLock::new();
    VALID_IDENT.get_or_init(|| {
        // One of:
        // - `*`
        // - An ident starting with `a-z_\$` and containing other characters `a-z0-9_\$`
        //
        // We could replace this with pomsky (regex<>pomsky : sql<>prql)
        // ^ ('*' | [ascii_lower '_$'] [ascii_lower ascii_digit '_$']* ) $
        regex::Regex::new(r"^((\*)|(^[a-z_\$][a-z0-9_\$]*))$").unwrap()
    })
}

fn is_keyword(ident: &str) -> bool {
    const KEYWORDS: &[&str] = &[
        "select", "from", "where", "group", "by", "limit", "offset", "distinct", "on", "none",
        "some", "end",
    ];
    KEYWORDS.contains(&ident)
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Ident {
            value: value.to_string(),
            quote_style: None,
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.quote_style {
            Some(q) if q == '"' || q == '\'' || q == '`' => {
                let escaped = string::escape(&self.value, q);
                write!(f, "{q}{escaped}{q}")
            }
            Some('[') => write!(f, "[{}]", self.value),
            None => f.write_str(&self.value),
            _ => panic!("unexpected quote style"),
        }
    }
}

/// A name of a table, view, custom type, etc., possibly multi-part, i.e. db.schema.obj
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ObjectName(pub Vec<Ident>);

impl From<Vec<Ident>> for ObjectName {
    fn from(idents: Vec<Ident>) -> Self {
        ObjectName(idents)
    }
}

impl fmt::Display for ObjectName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", display_separated(&self.0, "."))
    }
}

/// A WHEN clause in a CASE expression containing both
/// the condition and its corresponding result
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CaseWhen {
    pub condition: Expr,
    pub result: Expr,
}

impl fmt::Display for CaseWhen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("WHEN ")?;
        self.condition.fmt(f)?;
        f.write_str(" THEN")?;
        SpaceOrNewline.fmt(f)?;
        Indent(&self.result).fmt(f)?;
        Ok(())
    }
}

/// An SQL expression of any type.
///
/// # Semantics / Type Checking
///
/// The parser does not distinguish between expressions of different types
/// (e.g. boolean vs string). The caller is responsible for detecting and
/// validating types as necessary (for example  `WHERE 1` vs `SELECT 1=1`)
/// See the [README.md] for more details.
///
/// [README.md]: https://github.com/apache/datafusion-sqlparser-rs/blob/main/README.md#syntax-vs-semantics
///
/// # Equality and Hashing Does not Include Source Locations
///
/// The `Expr` type implements `PartialEq` and `Eq` based on the semantic value
/// of the expression (not bitwise comparison). This means that `Expr` instances
/// that are semantically equivalent but have different spans (locations in the
/// source tree) will compare as equal.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Expr {
    /// Direct SQL source
    Source(String),
    Identifier(Ident),
    CompoundIdentifier(Vec<Ident>),
    IndexBy(Option<Box<Expr>>),
    Case {
        operand: Option<Box<Expr>>,
        cases: Vec<CaseWhen>,
        else_result: Option<Box<Expr>>,
    },
    Subquery(Box<Query>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Source(s) => f.write_str(s),
            Expr::Identifier(s) => write!(f, "{s}"),
            Expr::CompoundIdentifier(s) => write!(f, "{}", display_separated(s, ".")),

            Expr::IndexBy(Some(order_by)) => {
                f.write_str("(ROW_NUMBER() OVER (ORDER BY ")?;
                order_by.fmt(f)?;
                f.write_str(")-1)::int4")
            }
            Expr::IndexBy(None) => f.write_str("(ROW_NUMBER() OVER ()-1)::int4"),

            Expr::Case {
                operand,
                cases,
                else_result,
            } => {
                f.write_str("CASE")?;
                if let Some(operand) = operand {
                    f.write_str(" ")?;
                    operand.fmt(f)?;
                }
                for case in cases {
                    SpaceOrNewline.fmt(f)?;
                    Indent(case).fmt(f)?;
                }
                if let Some(else_result) = else_result {
                    SpaceOrNewline.fmt(f)?;
                    Indent("ELSE").fmt(f)?;
                    SpaceOrNewline.fmt(f)?;
                    Indent(Indent(else_result)).fmt(f)?;
                }
                SpaceOrNewline.fmt(f)?;
                f.write_str("END")
            }
            Expr::Subquery(s) => {
                f.write_str("(")?;
                SpaceOrNewline.fmt(f)?;
                Indent(s).fmt(f)?;
                SpaceOrNewline.fmt(f)?;
                f.write_str(")")
            }
        }
    }
}
