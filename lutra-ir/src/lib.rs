mod literal;
mod parser;
mod printer;
mod sid;

pub mod ir {
    pub use crate::sid::SidKind;

    include!(concat!(env!("OUT_DIR"), "/project.rs"));
}

pub use parser::{_test_parse, parse};
pub use printer::print;

use lutra_frontend::pr;

pub fn ty_into_pr(ty: ir::Ty) -> pr::Ty {
    let kind = match ty.kind {
        ir::TyKind::Primitive(primitive) => {
            let primitive = match primitive {
                ir::PrimitiveSet::int => pr::PrimitiveSet::Int,
                ir::PrimitiveSet::float => pr::PrimitiveSet::Float,
                ir::PrimitiveSet::bool => pr::PrimitiveSet::Bool,
                ir::PrimitiveSet::text => pr::PrimitiveSet::Text,
            };
            pr::TyKind::Primitive(primitive)
        }
        ir::TyKind::Tuple(fields) => pr::TyKind::Tuple(
            fields
                .into_iter()
                .map(|f| {
                    let name = f.name.clone();
                    let ty = ty_into_pr(f.ty);
                    pr::TyTupleField { name, ty }
                })
                .collect(),
        ),
        ir::TyKind::Array(items_ty) => pr::TyKind::Array(Box::new(ty_into_pr(*items_ty))),
        ir::TyKind::Enum(variants) => pr::TyKind::Enum(
            variants
                .into_iter()
                .map(|v| pr::TyEnumVariant {
                    name: v.name,
                    ty: ty_into_pr(v.ty),
                })
                .collect(),
        ),
        ir::TyKind::Function(func) => pr::TyKind::Function(Some(pr::TyFunc {
            params: func.params.into_iter().map(ty_into_pr).map(Some).collect(),
            body: Some(Box::new(ty_into_pr(func.body))),
        })),
    };

    let mut head_size = ty.layout.head_size as usize;
    if head_size == 0 {
        if let Some(h) = kind.get_layout_simple() {
            head_size = h.head_size;
        }
    }

    let layout = Some(pr::TyLayout {
        head_size,
        variants_recursive: ty
            .layout
            .variants_recursive
            .iter()
            .map(|x| *x as usize)
            .collect(),
        body_ptr_offset: None, // TODO
    });

    pr::Ty {
        kind,
        span: None,
        name: None,
        layout,
    }
}
