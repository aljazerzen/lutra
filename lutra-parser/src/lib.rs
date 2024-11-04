pub mod error;
pub mod generic;
mod lexer;
mod parser;
pub mod span;
#[cfg(test)]
mod test;

pub use parser::pr;

pub fn parse_source(
    source: &str,
    source_id: u16,
) -> (Option<Vec<parser::pr::Stmt>>, Vec<error::Error>) {
    let (tokens, mut errors) = lexer::lex_source_recovery(source, source_id);

    let ast = if let Some(tokens) = tokens {
        let (ast, parse_errors) = parser::parse_lr_to_pr(source_id, tokens);
        errors.extend(parse_errors);
        ast
    } else {
        None
    };

    (ast, errors)
}
