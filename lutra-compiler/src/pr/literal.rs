use std::fmt::Write;

use enum_as_inner::EnumAsInner;

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Text(String),
    Date(Date),
    Time(Time),
    DateTime(Date, Time),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Date {
    pub year: i32,
    pub month: u8,
    pub day: u8,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Time {
    pub hours: i32,
    pub min: u8,
    pub sec: u8,
    pub micros: Option<u32>,
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

            Literal::Date(date) => {
                f.write_char('@')?;
                date.fmt(f)
            }
            Literal::Time(time) => {
                f.write_char('@')?;
                time.fmt(f)
            }
            Literal::DateTime(date, time) => {
                f.write_char('@')?;
                date.fmt(f)?;
                f.write_char('T')?;
                time.fmt(f)
            }
        }
    }
}

impl std::fmt::Display for Date {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Date { year, month, day } = self;
        write!(f, "{year:04}-{month:02}-{day:02}")
    }
}

impl Time {
    pub fn to_microseconds(&self) -> i64 {
        let h = self.hours.abs() as i64;
        let min = h * 60 + self.min as i64;
        let sec = min * 60 + self.sec as i64;
        let mut micros = sec * 1000000 + self.micros.unwrap_or_default() as i64;
        if self.hours < 0 {
            micros *= -1;
        }
        micros
    }
}

impl std::fmt::Display for Time {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Time {
            hours: hour,
            min,
            sec,
            micros: millis,
        } = self;
        write!(f, "{hour:02}:{min:02}:{sec:02}")?;
        if let Some(millis) = millis {
            write!(f, ".{millis:06}")?;
        }
        Ok(())
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
