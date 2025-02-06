use crate::string;

pub type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("type mismatch: expected {expected}, but found {found}")]
    TypeMismatch {
        expected: &'static str,
        found: string::String,
    },

    #[error("invalid data for given type")]
    InvalidData,

    #[error("invalid type: type has no binary format")]
    InvalidType,

    #[error("invalid type reference: {name}")]
    InvalidTypeReference { name: string::String },

    #[error("type has an infinite size")]
    InvalidTypeRecursive,
}
