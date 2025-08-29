pub type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("type error: expected {expected}, but found {found}")]
    BadValueType {
        expected: &'static str,
        found: &'static str,
    },

    #[error("invalid data for given type")]
    InvalidData,

    #[error("invalid type: type has no binary format")]
    InvalidType,
}
