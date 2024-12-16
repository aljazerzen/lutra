pub use crate::generated::ir::*;

impl Program {
    pub fn get_output_ty(&self) -> &Ty {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        &main_ty.body
    }

    pub fn get_input_tys(&self) -> &[Ty] {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        main_ty.params.as_slice()
    }
}

pub enum SidKind {
    External,
    Var,
    FunctionScope,
}

impl Sid {
    pub fn kind(&self) -> SidKind {
        let sid_kind: u32 = self.0 >> 30;
        match sid_kind {
            0 => SidKind::External,
            1 => SidKind::Var,
            2 => SidKind::FunctionScope,
            _ => {
                panic!()
            }
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{i}"),
            Literal::Float(i) => write!(f, "{i}"),
            Literal::Text(s) => {
                write!(f, "{}", quote_string(escape_all_except_quotes(s).as_str()))
            }
            Literal::Bool(b) => f.write_str(if *b { "true" } else { "false" }),
        }
    }
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind && self.name == other.name
    }
}

pub(crate) fn quote_string(s: &str) -> String {
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

pub(crate) fn escape_all_except_quotes(s: &str) -> String {
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
