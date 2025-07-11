//# Runner Representation of a program.

pub use crate::generated::rr::*;

pub struct TypedProgram<I: crate::Encode, O: crate::Decode> {
    pub inner: Program,
    input_ty: core::marker::PhantomData<I>,
    output_ty: core::marker::PhantomData<O>,
}

impl<I: crate::Encode, O: crate::Decode> From<Program> for TypedProgram<I, O> {
    fn from(inner: Program) -> Self {
        Self {
            inner,
            input_ty: core::marker::PhantomData,
            output_ty: core::marker::PhantomData,
        }
    }
}
