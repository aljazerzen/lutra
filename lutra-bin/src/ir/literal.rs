#![cfg(feature = "std")]

use crate::ir;

impl std::fmt::Display for ir::Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ir::Literal::bool(b) => f.write_str(if *b { "true" } else { "false" }),
            ir::Literal::int8(v) => write!(f, "{v}"),
            ir::Literal::int16(v) => write!(f, "{v}"),
            ir::Literal::int32(v) => write!(f, "{v}"),
            ir::Literal::int64(v) => write!(f, "{v}"),
            ir::Literal::uint8(v) => write!(f, "{v}"),
            ir::Literal::uint16(v) => write!(f, "{v}"),
            ir::Literal::uint32(v) => write!(f, "{v}"),
            ir::Literal::uint64(v) => write!(f, "{v}"),
            ir::Literal::float32(v) => write!(f, "{v}"),
            ir::Literal::float64(v) => write!(f, "{v}"),

            ir::Literal::text(s) => {
                write!(f, "{}", quote_string(escape_all_except_quotes(s).as_str()))
            }
        }
    }
}

fn quote_string(s: &str) -> String {
    if !s.contains('"') {
        return format!(r#""{s}""#);
    }

    if !s.contains('\'') {
        return format!("'{s}'");
    }

    // If the string starts or ends with a quote, use the other quote to delimit
    // the string. Otherwise default to double quotes.

    // TODO: this doesn't cover a string that starts with a single quote and
    // ends with a double quote; I think in that case it's necessary to escape
    // the quote. We need to add tests here.

    let quote = if s.starts_with('"') || s.ends_with('"') {
        '\''
    } else {
        '"'
    };

    // When string contains both single and double quotes find the longest
    // sequence of consecutive quotes, and then use the next highest odd number
    // of quotes (quotes must be odd; even number of quotes are empty strings).
    // i.e.:
    // 0 -> 1
    // 1 -> 3
    // 2 -> 3
    // 3 -> 5
    let max_consecutive = s
        .split(|c| c != quote)
        .map(|quote_sequence| quote_sequence.len())
        .max()
        .unwrap_or(0);
    let next_odd = max_consecutive.div_ceil(2) * 2 + 1;
    let delim = quote.to_string().repeat(next_odd);

    format!("{delim}{s}{delim}")
}

fn escape_all_except_quotes(s: &str) -> String {
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
