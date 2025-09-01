pub type Result<T, E = Error> = core::result::Result<T, E>;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Type error: expected {expected}, but found {found}")]
    BadValueType {
        expected: &'static str,
        found: &'static str,
    },

    #[error("Invalid data for given type")]
    InvalidData,

    #[error("Invalid type: type has no binary format")]
    InvalidType,

    #[error("Bug detected. Please file an issue.")]
    Bug,
}
