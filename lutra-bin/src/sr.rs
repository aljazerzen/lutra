//! SQL runner
//!
//! Contains program representation that is executable on SQL databases
//! and traits for running these programs.

use core::future::Future;

pub use crate::generated::sr::Program;

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

pub trait AsyncRun: Sized {
    type Error: core::fmt::Debug;

    fn run_binary(
        self,
        program: &Program,
        input: &[u8],
    ) -> impl Future<Output = Result<bytes::Bytes, Self::Error>>;

    fn run<I, O>(
        self,
        program: &TypedProgram<I, O>,
        input: &I,
    ) -> impl Future<Output = Result<O, Self::Error>>
    where
        I: crate::Encode,
        O: crate::Decode,
    {
        async {
            // encode inputs
            let mut input_buf = bytes::BytesMut::new();
            input.encode(&mut input_buf);

            // run
            let output = self.run_binary(&program.inner, &input_buf).await.unwrap();

            // decode output
            Ok(O::decode(&output).unwrap())
        }
    }
}

pub trait SyncRun: Sized {
    type Error: core::fmt::Debug;

    fn run_binary(self, program: &Program, input: &[u8]) -> Result<bytes::Bytes, Self::Error>;

    fn run<I, O>(self, program: &TypedProgram<I, O>, input: &I) -> Result<O, Self::Error>
    where
        I: crate::Encode,
        O: crate::Decode,
    {
        // encode inputs
        let mut input_buf = bytes::BytesMut::new();
        input.encode(&mut input_buf);

        // run
        let output = self.run_binary(&program.inner, &input_buf).unwrap();

        // decode output
        Ok(O::decode(&output).unwrap())
    }
}
