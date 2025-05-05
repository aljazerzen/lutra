#![cfg(test)]

mod lutra {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

mod func;
mod lbin;
mod lowering;
mod native_functions;
mod postgres;
mod resolver;
mod typed_data;

fn init_logger() {
    tracing_subscriber::fmt::Subscriber::builder()
        .without_time()
        .with_max_level(tracing::Level::DEBUG)
        .try_init()
        .ok();
}

fn normalize_expected(text: &str) -> std::borrow::Cow<'_, str> {
    let text = text.trim_start_matches('\n').trim_end_matches(['\n', ' ']);

    let min_indent = text
        .lines()
        .map(|line| line.chars().position(|c| c != ' ').unwrap_or(line.len()))
        .min()
        .unwrap_or_default();
    if min_indent == 0 {
        return std::borrow::Cow::Borrowed(text);
    }

    let unindented: Vec<String> = text
        .lines()
        .map(|l| l.chars().skip(min_indent).collect::<String>())
        .collect();
    std::borrow::Cow::Owned(unindented.join("\n"))
}
