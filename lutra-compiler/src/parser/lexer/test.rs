use chumsky::prelude::*;
use insta::assert_debug_snapshot;
use insta::assert_snapshot;

use crate::pr::Literal;

use super::{LError, lexer, literal, quoted_string};

#[track_caller]
fn _lex<'src, T>(
    parser: impl Parser<'src, &'src str, T, extra::Err<LError>>,
    source: &'src str,
) -> T {
    parser.parse(source).into_result().unwrap()
}

#[track_caller]
fn _lex_err<'src, T: std::fmt::Debug>(
    parser: impl Parser<'src, &'src str, T, extra::Err<LError>>,
    source: &'src str,
) -> Vec<LError> {
    parser.parse(source).into_result().unwrap_err()
}

#[test]
fn line_wrap() {
    assert_debug_snapshot!(_lex(lexer(), r"5 +
      3 "
        ), @r#"
    [
        0..1: Literal(Number("5")),
        2..3: Control('+'),
        3..4: NewLine,
        10..11: Literal(Number("3")),
    ]
    "#);

    assert_debug_snapshot!(_lex(lexer(), r"5 +
# comment
   # comment with whitespace
    3 "
        ), @r#"
    [
        0..1: Literal(Number("5")),
        2..3: Control('+'),
        3..4: NewLine,
        4..13: Comment("comment"),
        13..14: NewLine,
        17..42: Comment("comment with whitespace"),
        42..43: NewLine,
        47..48: Literal(Number("3")),
    ]
    "#);
}

#[test]
fn numbers() {
    // Binary notation
    assert_eq!(
        _lex(literal(), "0b1111000011110000"),
        Literal::Number("0b1111000011110000".into())
    );
    assert_eq!(
        _lex(literal(), "0b_1111000011110000"),
        Literal::Number("0b_1111000011110000".into())
    );

    // Hexadecimal notation
    assert_eq!(_lex(literal(), "0xff"), Literal::Number("0xff".into()));
    assert_eq!(
        _lex(literal(), "0x_deadbeef"),
        Literal::Number("0x_deadbeef".into())
    );

    // Octal notation
    assert_eq!(_lex(literal(), "0o777"), Literal::Number("0o777".into()));
}

#[test]
fn debug_display() {
    assert_debug_snapshot!(_lex(lexer(), "5 + 3"), @r#"
    [
        0..1: Literal(Number("5")),
        2..3: Control('+'),
        4..5: Literal(Number("3")),
    ]
    "#);
}

#[test]
fn doc_comment() {
    assert_debug_snapshot!(_lex(lexer(), "## docs"), @r#"
    [
        0..7: DocComment("docs"),
    ]
    "#);
}

#[test]
fn quotes() {
    // All these are valid & equal.
    assert_snapshot!(_lex(quoted_string(false), r#""aoeu""#), @"aoeu");
    assert_snapshot!(_lex(quoted_string(false), r#""""aoeu""""#), @"aoeu");
    assert_snapshot!(_lex(quoted_string(false), r#""""""aoeu""""""#), @"aoeu");
    assert_snapshot!(_lex(quoted_string(false), r#""""""""aoeu""""""""#), @"aoeu");

    // An even number is interpreted as a closed string (and the remainder is unparsed)
    assert_debug_snapshot!(_lex_err(quoted_string(false), r#"""aoeu"""#), @"
    [
        at 2..3,
    ]
    ");

    // When not escaping, we take the inner string between the three quotes
    assert_snapshot!(_lex(quoted_string(false), r#""""\"hello\""""#), @r#"\"hello\"#);

    assert_snapshot!(_lex(quoted_string(true), r#""""\"hello\"""""#), @r#""hello""#);

    // Escape each inner quote depending on the outer quote
    assert_snapshot!(_lex(quoted_string(true), r#""\"hello\"""#), @r#""hello""#);
    assert_snapshot!(_lex(quoted_string(true), r#""\"hello\"""#), @r#""hello""#);

    assert_snapshot!(_lex(quoted_string(true), r#""""#), @"");

    // An empty input should fail
    _lex_err(quoted_string(false), r#""#);

    // An even number of quotes is an empty string
    assert_snapshot!(_lex(quoted_string(true), r#""""""""#), @"");

    // Hex escape
    assert_snapshot!(_lex(quoted_string(true), r#""\x61\x62\x63""#), @"abc");

    // Unicode escape
    assert_snapshot!(_lex(quoted_string(true), r#""\u{01f422}""#), @"🐢");
}

#[test]
fn range() {
    assert_debug_snapshot!(_lex(lexer(), "1..2"), @r#"
    [
        0..1: Literal(Number("1")),
        1..3: Range,
        3..4: Literal(Number("2")),
    ]
    "#);

    assert_debug_snapshot!(_lex(lexer(), "..2"), @r#"
    [
        0..2: Range,
        2..3: Literal(Number("2")),
    ]
    "#);

    assert_debug_snapshot!(_lex(lexer(), "1.."), @r#"
    [
        0..1: Literal(Number("1")),
        1..3: Range,
    ]
    "#);

    assert_debug_snapshot!(_lex(lexer(), "in ..5"), @r#"
    [
        0..2: Keyword("in"),
        3..5: Range,
        5..6: Literal(Number("5")),
    ]
    "#);
}
