#![cfg(feature = "std")]
use std::collections::HashMap;

use crate::{TupleReader, ir};
use crate::{string::ToString, vec};

/// Utiliy for iterating over arbitrary data in tabular manner (as rows and columns).
pub struct Tabular<'d, 't> {
    inner: TableCell<'d, 't>,

    rem_items: usize,
    array_item_size: usize,

    types: HashMap<&'t ir::Path, &'t ir::Ty>,
}

#[derive(Clone, Copy)]
pub struct TableCell<'d, 't> {
    data: &'d [u8],
    ty: &'t ir::Ty,
    ty_defs: &'t [ir::TyDef],
}

impl<'d, 't> TableCell<'d, 't> {
    pub fn data(&self) -> &'d [u8] {
        self.data
    }

    pub fn ty(&self) -> &'t ir::Ty {
        self.ty
    }

    pub fn ty_defs(&self) -> &'t [ir::TyDef] {
        self.ty_defs
    }
}

impl<'d, 't> Tabular<'d, 't> {
    pub fn new(data: &'d [u8], ty: &'t ir::Ty, ty_defs: &'t [ir::TyDef]) -> Self {
        let mut r = Tabular {
            inner: TableCell { data, ty, ty_defs },
            rem_items: 0,
            array_item_size: 0,
            types: HashMap::from_iter(ty_defs.iter().map(|d| (&d.name, &d.ty))),
        };

        match &r.get_ty_mat(ty).kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Tuple(_) | ir::TyKind::Enum(_) => {
                r.rem_items = 1;
            }
            ir::TyKind::Array(item) => {
                let (offset, len) =
                    crate::ArrayReader::<&[u8]>::read_head(TableCell { data, ty, ty_defs }.data);
                r.inner.data = &TableCell { data, ty, ty_defs }.data[offset..];
                r.rem_items = len;
                r.array_item_size = item.layout.as_ref().unwrap().head_size.div_ceil(8) as usize;
            }
            ir::TyKind::Function(_) => unreachable!(),
            ir::TyKind::Ident(_) => todo!(),
        }
        r
    }

    fn get_ty_mat(&self, ty: &'t ir::Ty) -> &'t ir::Ty {
        match &ty.kind {
            ir::TyKind::Ident(path) => self.types.get(path).unwrap(),
            _ => ty,
        }
    }

    pub fn column_names(&self) -> Vec<String> {
        self.column_names_of_ty(self.inner.ty)
    }
    fn column_names_of_ty(&self, ty: &ir::Ty) -> Vec<String> {
        match &self.get_ty_mat(ty).kind {
            // arrays are iterated over, columns come from inner type
            ir::TyKind::Array(item) => self.column_names_of_ty(item),

            // tuple fields become columns
            ir::TyKind::Tuple(fields) => fields
                .iter()
                .enumerate()
                .map(|(i, f)| {
                    if let Some(name) = &f.name {
                        name.clone()
                    } else {
                        i.to_string()
                    }
                })
                .collect(),

            // primitives become a single column
            // (we also infer name from ident)
            ir::TyKind::Primitive(_) => {
                if let ir::TyKind::Ident(path) = &ty.kind {
                    vec![path.0.last().unwrap().clone()]
                } else {
                    vec!["value".into()]
                }
            }
            ir::TyKind::Enum(_) => todo!(),
            ir::TyKind::Ident(_) | ir::TyKind::Function(_) => unreachable!(),
        }
    }
}

impl<'d, 't> Iterator for Tabular<'d, 't> {
    type Item = vec::Vec<TableCell<'d, 't>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.rem_items == 0 {
            return None;
        }
        let mut row = self.inner;
        if let ir::TyKind::Array(item) = &row.ty.kind {
            row.ty = item.as_ref();
        }

        // advance
        self.rem_items -= 1;
        if let ir::TyKind::Array(_) = &self.inner.ty.kind {
            self.inner.data = &self.inner.data[self.array_item_size..];
        }

        // unpack row
        let row_ty_mat = self.get_ty_mat(row.ty);
        Some(match &row_ty_mat.kind {
            ir::TyKind::Primitive(_) | ir::TyKind::Array(_) => {
                vec![row]
            }
            ir::TyKind::Tuple(fields) => {
                let mut cells = Vec::with_capacity(fields.len());
                let reader = TupleReader::new_for_ty(row.data, row_ty_mat);
                for (i, f) in fields.iter().enumerate() {
                    cells.push(TableCell {
                        data: reader.get_field(i),
                        ty: &f.ty,
                        ty_defs: row.ty_defs,
                    })
                }
                cells
            }
            ir::TyKind::Enum(_) => todo!(),
            ir::TyKind::Function(_) | ir::TyKind::Ident(_) => unreachable!(),
        })
    }
}
