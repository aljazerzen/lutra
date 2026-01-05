use insta::assert_debug_snapshot;
use insta::assert_snapshot;

use crate::pr::Literal;

use super::{LError, lexer, literal, quoted_string};

fn lex<T>(
    lexer: impl chumsky::Parser<char, T, Error = LError>,
    source: &str,
) -> Result<T, Vec<LError>> {
    lexer.parse(super::prepare_stream(source))
}

#[test]
fn line_wrap() {
    assert_debug_snapshot!(lex(lexer(), r"5 +
      3 "
        ).unwrap(), @r#"
    [
        0..1: Literal(Number("5")),
        2..3: Control('+'),
        3..4: NewLine,
        10..11: Literal(Number("3")),
    ]
    "#);

    assert_debug_snapshot!(lex(lexer(), r"5 +
# comment
   # comment with whitespace
    3 "
        ).unwrap(), @r#"
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
        lex(literal(), "0b1111000011110000").unwrap(),
        Literal::Number("0b1111000011110000".into())
    );
    assert_eq!(
        lex(literal(), "0b_1111000011110000").unwrap(),
        Literal::Number("0b_1111000011110000".into())
    );

    // Hexadecimal notation
    assert_eq!(
        lex(literal(), "0xff").unwrap(),
        Literal::Number("0xff".into())
    );
    assert_eq!(
        lex(literal(), "0x_deadbeef").unwrap(),
        Literal::Number("0x_deadbeef".into())
    );

    // Octal notation
    assert_eq!(
        lex(literal(), "0o777").unwrap(),
        Literal::Number("0o777".into())
    );
}

#[test]
fn debug_display() {
    assert_debug_snapshot!(lex(lexer(), "5 + 3").unwrap(), @r#"
    [
        0..1: Literal(Number("5")),
        2..3: Control('+'),
        4..5: Literal(Number("3")),
    ]
    "#);
}

#[test]
fn doc_comment() {
    assert_debug_snapshot!(lex(lexer(), "## docs").unwrap(), @r#"
    [
        0..7: DocComment("docs"),
    ]
    "#);
}

#[test]
fn quotes() {
    // All these are valid & equal.
    assert_snapshot!(lex(quoted_string(false), r#""aoeu""#).unwrap(), @"aoeu");
    assert_snapshot!(lex(quoted_string(false), r#""""aoeu""""#).unwrap(), @"aoeu");
    assert_snapshot!(lex(quoted_string(false), r#""""""aoeu""""""#).unwrap(), @"aoeu");
    assert_snapshot!(lex(quoted_string(false), r#""""""""aoeu""""""""#).unwrap(), @"aoeu");

    // An even number is interpreted as a closed string (and the remainder is unparsed)
    assert_snapshot!(lex(quoted_string(false), r#"""aoeu"""#).unwrap(), @"");

    // When not escaping, we take the inner string between the three quotes
    assert_snapshot!(lex(quoted_string(false), r#""""\"hello\""""#).unwrap(), @r#"\"hello\"#);

    assert_snapshot!(lex(quoted_string(true), r#""""\"hello\"""""#).unwrap(), @r#""hello""#);

    // Escape each inner quote depending on the outer quote
    assert_snapshot!(lex(quoted_string(true), r#""\"hello\"""#).unwrap(), @r#""hello""#);
    assert_snapshot!(lex(quoted_string(true), r#""\"hello\"""#).unwrap(), @r#""hello""#);

    assert_snapshot!(lex(quoted_string(true), r#""""#).unwrap(), @"");

    // An empty input should fail
    lex(quoted_string(false), r#""#).unwrap_err();

    // An even number of quotes is an empty string
    assert_snapshot!(lex(quoted_string(true), r#""""""""#).unwrap(), @"");

    // Hex escape
    assert_snapshot!(lex(quoted_string(true), r#""\x61\x62\x63""#).unwrap(), @"abc");

    // Unicode escape
    assert_snapshot!(lex(quoted_string(true), r#""\u{01f422}""#).unwrap(), @"🐢");
}

#[test]
fn range() {
    assert_debug_snapshot!(lex(lexer(), "1..2").unwrap(), @r#"
    [
        0..1: Literal(Number("1")),
        1..3: Range,
        3..4: Literal(Number("2")),
    ]
    "#);

    assert_debug_snapshot!(lex(lexer(), "..2").unwrap(), @r#"
    [
        0..2: Range,
        2..3: Literal(Number("2")),
    ]
    "#);

    assert_debug_snapshot!(lex(lexer(), "1..").unwrap(), @r#"
    [
        0..1: Literal(Number("1")),
        1..3: Range,
    ]
    "#);

    assert_debug_snapshot!(lex(lexer(), "in ..5").unwrap(), @r#"
    [
        0..2: Keyword("in"),
        3..5: Range,
        5..6: Literal(Number("5")),
    ]
    "#);
}
