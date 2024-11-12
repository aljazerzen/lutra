#![cfg(test)]

mod lutra {
    include!(concat!(env!("OUT_DIR"), "/lutra.rs"));
}

mod bin;
mod runtime;
mod typed_data;
