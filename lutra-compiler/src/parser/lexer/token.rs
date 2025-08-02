use crate::pr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tokens(pub Vec<Token>);

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: std::ops::Range<usize>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum TokenKind {
    Ident(String),
    Keyword(&'static str),
    Literal(pr::Literal),

    Interpolation(char, String),

    /// single-char control tokens
    Control(char),

    ArrowThin,   // ->
    ArrowFat,    // =>
    Eq,          // ==
    Ne,          // !=
    Gte,         // >=
    Lte,         // <=
    RegexSearch, // ~=
    And,         // &&
    Or,          // ||
    Coalesce,    // ??
    DivInt,      // //
    Pow,         // **
    PathSep,     // ::
    Range,       // ..

    DocComment(String),
    DocCommentSelf(String),
}

// This is here because Literal::Float(f64) does not implement Hash, so we cannot simply derive it.
// There are reasons for that, but chumsky::Error needs Hash for the TokenKind, so it can deduplicate
// tokens in error.
// So this hack could lead to duplicated tokens in error messages. Oh no.
#[allow(clippy::derived_hash_with_manual_eq)]
impl std::hash::Hash for TokenKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl std::cmp::Eq for TokenKind {}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Ident(s) => {
                if s.is_empty() {
                    // FYI this shows up in errors
                    write!(f, "an identifier")
                } else {
                    write!(f, "{s}")
                }
            }
            TokenKind::Keyword(s) => write!(f, "keyword {s}"),
            TokenKind::Literal(lit) => write!(f, "{lit}"),
            TokenKind::Control(c) => write!(f, "{c}"),

            TokenKind::ArrowThin => f.write_str("->"),
            TokenKind::ArrowFat => f.write_str("=>"),
            TokenKind::Eq => f.write_str("=="),
            TokenKind::Ne => f.write_str("!="),
            TokenKind::Gte => f.write_str(">="),
            TokenKind::Lte => f.write_str("<="),
            TokenKind::RegexSearch => f.write_str("~="),
            TokenKind::And => f.write_str("&&"),
            TokenKind::Or => f.write_str("||"),
            TokenKind::Coalesce => f.write_str("??"),
            TokenKind::DivInt => f.write_str("//"),
            TokenKind::Pow => f.write_str("**"),
            TokenKind::PathSep => f.write_str("::"),
            TokenKind::Range => f.write_str(".."),

            TokenKind::Interpolation(c, s) => {
                write!(f, "{c}\"{s}\"")
            }
            TokenKind::DocComment(text) => writeln!(f, "##{text}"),
            TokenKind::DocCommentSelf(text) => writeln!(f, "#!{text}"),
        }
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}..{}: {:?}", self.span.start, self.span.end, self.kind)
    }
}

#[cfg(test)]
mod test {
    use insta::assert_snapshot;
    use pr::Literal;

    use super::*;

    #[test]
    fn test_string_quoting() {
        fn make_str(s: &str) -> Literal {
            Literal::Text(s.to_string())
        }

        assert_snapshot!(
            make_str("hello").to_string(),
            @r###""hello""###
        );

        assert_snapshot!(
            make_str(r#"he's nice"#).to_string(),
            @r###""he's nice""###
        );

        assert_snapshot!(
            make_str(r#"he said "what up""#).to_string(),
            @r#""""he said "what up"""""#
        );

        assert_snapshot!(
            make_str(r#"he said "what's up""#).to_string(),
            @r#""""he said "what's up"""""#
        );

        assert_snapshot!(
            make_str(r#" single' three double""" four double"""" "#).to_string(),
            @r#"""""" single' three double""" four double"""" """"""#

        );

        assert_snapshot!(
            make_str(r#""Starts with a double quote and ' contains a single quote"#).to_string(),
            @r#"""""Starts with a double quote and ' contains a single quote""""#
        );
    }

    #[test]
    fn test_string_escapes() {
        assert_snapshot!(
            Literal::Text(r#"hello\nworld"#.to_string()).to_string(),
            @r###""hello\\nworld""###
        );

        assert_snapshot!(
            Literal::Text(r#"hello\tworld"#.to_string()).to_string(),
            @r###""hello\\tworld""###
        );

        // TODO: one problem here is that we don't remember whether the original
        // string contained an actual line break or contained an `\n` string,
        // because we immediately normalize both to `\n`. This means that when
        // we format the PRQL, we can't retain the original. I think three ways of
        // resolving this:
        // - Have different tokens in the lexer and parser; normalize at the
        //   parsing stage, and then use the token in the lexer for writing out
        //   the formatted PRQL. Literals are one of the only data structures we
        //   retain between the lexer and parser. (note that this requires the
        //   current effort to use tokens from the lexer as part of `prqlc fmt`;
        //   ongoing as of 2024-08)
        // - Don't normalize at all, and then normalize when we use the string.
        //   I think this might be viable and maybe easy, but is a bit less
        //   elegant; the parser is designed to normalize this sort of thing.

        assert_snapshot!(
            Literal::Text(r#"hello
            world"#.to_string()).to_string(),
            @r###""hello\n            world""###
        );
    }

    #[test]
    fn test_raw_string_quoting() {
        // TODO: add some test for escapes
        fn make_str(s: &str) -> Literal {
            Literal::Text(s.to_string())
        }

        assert_snapshot!(
            make_str("hello").to_string(),
            @r#""hello""#
        );
    }
}
