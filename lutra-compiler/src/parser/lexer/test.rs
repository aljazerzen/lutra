use insta::assert_debug_snapshot;
use insta::assert_snapshot;

use crate::pr::Literal;

use super::{LError, lex_source, lexer, literal, quoted_string};

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
        ).unwrap(), @r"
    [
        0..1: Literal(Integer(5)),
        2..3: Control('+'),
        10..11: Literal(Integer(3)),
    ]
    ");

    assert_debug_snapshot!(lex(lexer(), r"5 +
# comment
   # comment with whitespace
    3 "
        ).unwrap(), @r"
    [
        0..1: Literal(Integer(5)),
        2..3: Control('+'),
        47..48: Literal(Integer(3)),
    ]
    ");
}

#[test]
fn numbers() {
    // Binary notation
    assert_eq!(
        lex(literal(), "0b1111000011110000").unwrap(),
        Literal::Integer(61680)
    );
    assert_eq!(
        lex(literal(), "0b_1111000011110000").unwrap(),
        Literal::Integer(61680)
    );

    // Hexadecimal notation
    assert_eq!(lex(literal(), "0xff").unwrap(), Literal::Integer(255));
    assert_eq!(
        lex(literal(), "0x_deadbeef").unwrap(),
        Literal::Integer(3735928559)
    );

    // Octal notation
    assert_eq!(lex(literal(), "0o777").unwrap(), Literal::Integer(511));
}

#[test]
fn debug_display() {
    assert_debug_snapshot!(lex(lexer(), "5 + 3").unwrap(), @r"
    [
        0..1: Literal(Integer(5)),
        2..3: Control('+'),
        4..5: Literal(Integer(3)),
    ]
    ");
}

#[test]
fn doc_comment() {
    assert_debug_snapshot!(lex(lexer(), "## docs").unwrap(), @r#"
    [
        0..7: DocComment(" docs"),
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
    assert_debug_snapshot!(lex(lexer(), "1..2").unwrap(), @r"

        [
            0..1: Literal(Integer(1)),
            1..3: Range,
            3..4: Literal(Integer(2)),
        ],
    )
    ");

    assert_debug_snapshot!(lex(lexer(), "..2").unwrap(), @r"

        [
            0..2: Range,
            2..3: Literal(Integer(2)),
        ],
    )
    ");

    assert_debug_snapshot!(lex(lexer(), "1..").unwrap(), @r"

        [
            0..1: Literal(Integer(1)),
            1..3: Range,
        ],
    )
    ");

    assert_debug_snapshot!(lex(lexer(), "in ..5").unwrap(), @r#"

        [
            0..2: Ident("in"),
            3..5: Range,
            5..6: Literal(Integer(5)),
        ],
    )
    "#);
}

#[test]
fn test_lex_source() {
    use insta::assert_debug_snapshot;

    assert_debug_snapshot!(lex_source("5 + 3"), @r"
    Ok(
        [
            0..1: Literal(Integer(5)),
            2..3: Control('+'),
            4..5: Literal(Integer(3)),
        ],
    )
    ");

    // Something that will generate an error
    assert_debug_snapshot!(lex_source("^"), @r#"
    Err(
        [
            Diagnostic {
                code: DiagnosticCode(
                    "E0003",
                ),
                message: "unexpected ^",
                span: Some(
                    0:0-1,
                ),
                additional: [],
            },
        ],
    )
    "#);
}
