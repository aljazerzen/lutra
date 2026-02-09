use super::Table;
use crate::ir;
use crate::{TableCell, TupleReader};

impl<'d, 't> Iterator for Table<'d, 't> {
    type Item = Vec<TableCell<'d, 't>>;

    fn next(&mut self) -> Option<Self::Item> {
        let row = self.reader.next()?;
        Some(self.flatten_row(row))
    }
}

impl<'d, 't> Table<'d, 't> {
    fn flatten_row(&self, cells: Vec<TableCell<'d, 't>>) -> Vec<TableCell<'d, 't>> {
        let mut leaves = Vec::new();
        for cell in cells {
            self.flatten_cell(cell, &mut leaves);
        }
        leaves
    }

    fn flatten_cell(&self, cell: TableCell<'d, 't>, out: &mut Vec<TableCell<'d, 't>>) {
        let ty = self.get_ty_mat(cell.ty());
        match &ty.kind {
            ir::TyKind::Tuple(fields) => {
                let reader = TupleReader::new_for_ty(cell.data(), ty);
                for (i, field) in fields.iter().enumerate() {
                    let field_data: &'d [u8] = reader.get_field(i);
                    let field_cell = TableCell::new(field_data, &field.ty, cell.ty_defs());
                    self.flatten_cell(field_cell, out);
                }
            }
            _ => {
                // Leaf: primitive, array, or enum
                out.push(cell);
            }
        }
    }
}
