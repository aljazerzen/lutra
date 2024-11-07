use enum_as_inner::EnumAsInner;

#[derive(Debug, EnumAsInner, PartialEq, Clone, strum::AsRefStr)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    RawString(String),
    Date(String),
    Time(String),
    Timestamp(String),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(i) => write!(f, "{i}")?,
            Literal::Float(i) => write!(f, "{i}")?,

            Literal::String(s) => {
                write!(f, "{}", quote_string(escape_all_except_quotes(s).as_str()))?;
            }

            Literal::RawString(s) => {
                write!(f, "r{}", quote_string(s))?;
            }

            Literal::Boolean(b) => {
                f.write_str(if *b { "true" } else { "false" })?;
            }

            Literal::Date(inner) | Literal::Time(inner) | Literal::Timestamp(inner) => {
                write!(f, "@{inner}")?;
            }
        }
        Ok(())
    }
}

fn quote_string(s: &str) -> String {
    if !s.contains('"') {
        return format!(r#""{}""#, s);
    }

    if !s.contains('\'') {
        return format!("'{}'", s);
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
    let next_odd = (max_consecutive + 1) / 2 * 2 + 1;
    let delim = quote.to_string().repeat(next_odd);

    format!("{}{}{}", delim, s, delim)
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
