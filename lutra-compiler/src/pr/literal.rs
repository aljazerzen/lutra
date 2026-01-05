use std::fmt::Write;

use enum_as_inner::EnumAsInner;

#[derive(Debug, EnumAsInner, PartialEq, Clone)]
pub enum Literal {
    Number(String),
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

impl Literal {
    pub fn as_integer(&self) -> Option<u64> {
        let mut number = self.as_number()?.clone();
        number.retain(|c| c != '_');

        if let Some(digits) = number.strip_prefix("0x") {
            return u64::from_str_radix(digits, 16).ok();
        }
        if let Some(digits) = number.strip_prefix("0o") {
            return u64::from_str_radix(digits, 8).ok();
        }
        if let Some(digits) = number.strip_prefix("0b") {
            return u64::from_str_radix(digits, 2).ok();
        }
        if let Ok(unsigned) = number.parse::<u64>() {
            return Some(unsigned);
        }
        if let Ok(signed) = number.parse::<i64>() {
            return Some(signed as u64);
        }
        None
    }
    pub fn as_float(&self) -> Option<f64> {
        let mut number = self.as_number()?.clone();
        number.retain(|c| c != '_');
        number.parse::<f64>().ok()
    }
    pub fn as_decimal(&self) -> Option<i64> {
        let number = self.as_number()?;
        if number.contains(|c: char| !(c.is_ascii_digit() || c == '_' || c == '.')) {
            return None;
        }

        // detach minus
        let (minus, digits) = number
            .strip_prefix("-")
            .map(|n| (true, n))
            .unwrap_or((false, number));

        // determine scale
        let scale = digits
            .bytes()
            .skip_while(|c| *c != b'.')
            .skip(1)
            .filter(|c| *c != b'_')
            .count();
        if scale > 2 {
            return None;
        }

        // parse digits
        let mut r = digits
            .bytes()
            .filter(|c| *c != b'_' && *c != b'.')
            .try_fold(0_i64, |v, d| {
                v.checked_mul(10)?.checked_add((d - b'0') as i64)
            })?;

        // bring to scale 2
        r = r.checked_mul(10_i64.pow(2 - scale as u32))?;

        // apply minus
        if minus {
            r = r.checked_neg()?
        }

        Some(r)
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(i) => write!(f, "{i}"),

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

impl Date {
    pub fn to_epoch_days(&self) -> Option<i32> {
        chrono::NaiveDate::from_ymd_opt(self.year, self.month as u32, self.day as u32)
            .map(|d| d.to_epoch_days())
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
