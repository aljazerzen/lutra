#![cfg(test)]

mod lutra {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

mod e2e;
mod lbin;
mod lowering;
mod native_functions;
mod sql;
mod typed_data;
mod types;
