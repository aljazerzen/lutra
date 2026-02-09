#![cfg(test)]

mod lutra {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

/// URL of a PostgreSQL database with permissions to create new databases.
const POSTGRES_URL_SHARED: &str = "postgresql://postgres:pass@localhost:5416";

mod duckdb;
mod fuzz;
mod inliner;
mod interpreter;
mod lbin;
mod lowerer;
mod native_functions;
mod postgres;
mod resolver;
mod runner;
mod table;
mod typed_data;

fn init_logger() {
    tracing_subscriber::fmt::Subscriber::builder()
        .without_time()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_target(false)
        .try_init()
        .ok();

    // this is for making similar-asserts use colors
    console::set_colors_enabled(true);
}
