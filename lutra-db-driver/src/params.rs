use bytes::BufMut;
use lutra_bin::{sr, Decode};
use postgres_types as pg_ty;

pub fn to_sql<'d>(program: &sr::Program, input: &'d [u8]) -> Args<'d> {
    let mut args = Vec::with_capacity(program.input_tys.len());
    let mut offset = 0;
    for input_ty in &program.input_tys {
        args.push(Arg {
            data: &input[offset..],
        });
        offset += input_ty.layout.as_ref().unwrap().head_size.div_ceil(8) as usize;
    }

    Args { args }
}

pub struct Args<'a> {
    args: Vec<Arg<'a>>,
}

impl<'a> Args<'a> {
    pub fn as_refs(&self) -> Vec<&(dyn pg_ty::ToSql + Sync)> {
        self.args
            .iter()
            .map(|x| x as &(dyn pg_ty::ToSql + Sync))
            .collect()
    }
}

#[derive(Debug)]
struct Arg<'a> {
    data: &'a [u8],
}

impl<'a> pg_ty::ToSql for Arg<'a> {
    fn to_sql(
        &self,
        ty: &pg_ty::Type,
        out: &mut bytes::BytesMut,
    ) -> Result<pg_ty::IsNull, Box<dyn std::error::Error + Sync + Send>>
    where
        Self: Sized,
    {
        match ty.name() {
            "bool" => out.put_slice(&self.data[0..1]),
            "int1" => todo!(),
            "int2" => out.extend(self.data[0..2].iter().rev()),
            "int4" => out.extend(self.data[0..4].iter().rev()),
            "int8" => out.extend(self.data[0..8].iter().rev()),
            "uint8" => todo!(),
            "uint16" => todo!(),
            "uint32" => todo!(),
            "uint64" => todo!(),
            "float32" => out.put_slice(&self.data[0..4]),
            "float64" => out.put_slice(&self.data[0..8]),
            "text" => {
                let val = String::decode(self.data)?;
                out.put_slice(val.as_bytes())
            }
            _ => todo!(),
        }
        Ok(pg_ty::IsNull::No)
    }

    fn accepts(_ty: &pg_ty::Type) -> bool {
        true
    }

    pg_ty::to_sql_checked!();
}
