mod generated;
mod literal;
mod sid;

pub use generated::*;
pub use sid::SidKind;

impl Program {
    pub fn get_output_ty(&self) -> &Ty {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        &main_ty.body
    }

    pub fn get_input_tys(&self) -> &[Ty] {
        let main_ty = self.main.ty.kind.as_function().unwrap();
        main_ty.params.as_slice()
    }
}
