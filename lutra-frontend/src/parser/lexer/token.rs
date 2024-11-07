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
    NewLine,

    Ident(String),
    Keyword(String),
    Literal(pr::Literal),
    /// A parameter such as `$1`
    Param(String),

    Range {
        /// Whether the left side of the range is bound by the previous token
        /// (but it's not contained in this token)
        bind_left: bool,
        bind_right: bool,
    },
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
    Annotate,    // @
    PathSep,     // ::

    // Aesthetics only
    Comment(String),
    DocComment(String),
    /// Vec containing comments between the newline and the line wrap
    // Currently we include the comments with the LineWrap token. This isn't
    // ideal, but I'm not sure of an easy way of having them be separate.
    // - The line wrap span technically includes the comments — on a newline,
    //   we need to look ahead to _after_ the comments to see if there's a
    //   line wrap, and exclude the newline if there is.
    // - We can only pass one token back
    //
    // Alternatives:
    // - Post-process the stream, removing the newline prior to a line wrap.
    //   But requires a whole extra pass.
    // - Change the functionality. But it's very nice to be able to comment
    //   something out and have line-wraps still work.
    LineWrap(Vec<TokenKind>),

    /// A token we manually insert at the start of the input, which later stages
    /// can treat as a newline.
    Start,
}

impl TokenKind {
    pub fn range(bind_left: bool, bind_right: bool) -> Self {
        TokenKind::Range {
            bind_left,
            bind_right,
        }
    }
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
            TokenKind::NewLine => write!(f, "new line"),
            TokenKind::Ident(s) => {
                if s.is_empty() {
                    // FYI this shows up in errors
                    write!(f, "an identifier")
                } else {
                    write!(f, "{s}")
                }
            }
            TokenKind::Keyword(s) => write!(f, "keyword {s}"),
            TokenKind::Literal(lit) => write!(f, "{}", lit),
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
            TokenKind::Annotate => f.write_str("@"),
            TokenKind::PathSep => f.write_str("::"),

            TokenKind::Param(id) => write!(f, "${id}"),

            TokenKind::Range {
                bind_left,
                bind_right,
            } => write!(
                f,
                "'{}..{}'",
                if *bind_left { "" } else { " " },
                if *bind_right { "" } else { " " }
            ),
            TokenKind::Interpolation(c, s) => {
                write!(f, "{c}\"{}\"", s)
            }
            TokenKind::Comment(s) => {
                writeln!(f, "#{}", s)
            }
            TokenKind::DocComment(s) => {
                writeln!(f, "#!{}", s)
            }
            TokenKind::LineWrap(comments) => {
                write!(f, "\n\\ ")?;
                for comment in comments {
                    write!(f, "{}", comment)?;
                }
                Ok(())
            }
            TokenKind::Start => write!(f, "start of input"),
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
            Literal::String(s.to_string())
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
            @r###"'he said "what up"'"###
        );

        assert_snapshot!(
            make_str(r#"he said "what's up""#).to_string(),
            @r###"'''he said "what's up"'''"###
        );

        assert_snapshot!(
            make_str(r#" single' three double""" four double"""" "#).to_string(),
            @r###"""""" single' three double""" four double"""" """"""###

        );

        assert_snapshot!(
            make_str(r#""Starts with a double quote and ' contains a single quote"#).to_string(),
            @r###"'''"Starts with a double quote and ' contains a single quote'''"###
        );
    }

    #[test]
    fn test_string_escapes() {
        assert_snapshot!(
            Literal::String(r#"hello\nworld"#.to_string()).to_string(),
            @r###""hello\\nworld""###
        );

        assert_snapshot!(
            Literal::String(r#"hello\tworld"#.to_string()).to_string(),
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
            Literal::String(r#"hello
            world"#.to_string()).to_string(),
            @r###""hello\n            world""###
        );
    }

    #[test]
    fn test_raw_string_quoting() {
        // TODO: add some test for escapes
        fn make_str(s: &str) -> Literal {
            Literal::RawString(s.to_string())
        }

        assert_snapshot!(
            make_str("hello").to_string(),
            @r###"r"hello""###
        );
    }
}
