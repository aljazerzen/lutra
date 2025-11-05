use enum_as_inner::EnumAsInner;

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Text(String),
    Date(String),
    Time(String),
    Timestamp(String),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{i}"),
            Literal::Float(n) => std::fmt::Debug::fmt(n, f),

            Literal::Text(s) => {
                write!(f, "{}", quote_string(escape_all_except_quotes(s).as_str()))
            }

            Literal::Boolean(b) => f.write_str(if *b { "true" } else { "false" }),

            Literal::Date(inner) | Literal::Time(inner) | Literal::Timestamp(inner) => {
                write!(f, "@{inner}")
            }
        }
    }
}

fn quote_string(s: &str) -> String {
    if !s.contains('"') {
        return format!(r#""{s}""#);
    }

    // When string contains quotes find the longest sequence of consecutive quotes,
    // and then use the next highest odd number of quotes
    // (quotes must be odd; even number of quotes are empty strings).
    // i.e.:
    // 0 -> 1
    // 1 -> 3
    // 2 -> 3
    // 3 -> 5
    let max_consecutive = s
        .split(|c| c != '"')
        .map(|quote_sequence| quote_sequence.len())
        .max()
        .unwrap_or(0);
    let next_odd = max_consecutive.div_ceil(2) * 2 + 1;
    let delim = '"'.to_string().repeat(next_odd);

    format!("{delim}{s}{delim}")
}

pub fn escape_all_except_quotes(s: &str) -> String {
    let mut result = String::new();
    for ch in s.chars() {
        if ch == '"' || ch == '\'' {
            result.push(ch);
        } else {
            result.extend(ch.escape_default());
        }
    }
    result
}

#[test]
#[cfg(test)]
fn test_display_literal() {
    assert_eq!(
        Literal::Integer(i64::MAX).to_string(),
        "9223372036854775807"
    );
    assert_eq!(
        Literal::Integer(i64::MIN).to_string(),
        "-9223372036854775808"
    );
    assert_eq!(Literal::Float(10.00000).to_string(), "10.0");
    assert_eq!(Literal::Float(004.0232323).to_string(), "4.0232323");
}
