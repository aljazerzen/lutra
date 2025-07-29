#![cfg(test)]

mod lutra {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

mod inliner;
mod interpreter;
mod lbin;
mod lowerer;
mod native_functions;
mod postgres;
mod resolver;
mod runner;
mod typed_data;

fn init_logger() {
    tracing_subscriber::fmt::Subscriber::builder()
        .without_time()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .try_init()
        .ok();

    // this is for making similar-asserts use colors
    console::set_colors_enabled(true);
}
