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
        .with_max_level(tracing::Level::TRACE)
        .try_init()
        .ok();
}
