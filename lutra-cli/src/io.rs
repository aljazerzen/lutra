use std::io::{self, BufReader, BufWriter, Read, Write};
use std::path;
use std::str::FromStr;

use lutra_bin::{bytes, ir, rr, typed_data};

use crate::DataFormat;

pub fn write_output(
    data: &[u8],
    output_path: Option<path::PathBuf>,
    cmd_format: Option<DataFormat>,
    ty: &rr::ProgramType,
) -> anyhow::Result<()> {
    let format = cmd_format
        // try to infer from output path extension
        .or_else(|| {
            output_path
                .as_ref()
                .and_then(|p| p.extension())
                .and_then(|ex| ex.to_str())
                .and_then(|ex| DataFormat::from_str(ex).ok())
        })
        // fallback to lt
        .unwrap_or(DataFormat::Lt);

    if let Some(output_path) = &output_path {
        // file
        let mut writer = BufWriter::new(std::fs::File::create(output_path)?);
        let size = write_format(&mut writer, data, format, &ty.output, &ty.defs)?;
        writer.flush()?;
        eprintln!(
            "Output written to {} ({}, {size} bytes)",
            output_path.display(),
            format.as_ref(),
        );
    } else {
        // stdout
        let mut writer = BufWriter::new(io::stdout());
        write_format(&mut writer, data, format, &ty.output, &ty.defs)?;
        writer.flush()?;
    };

    Ok(())
}

fn write_format(
    w: &mut impl Write,
    data: &[u8],
    format: DataFormat,
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> anyhow::Result<usize> {
    let mut w = WriteCounter::new(w);

    match format {
        DataFormat::Ld => w.write_all(data)?,

        DataFormat::Lt => {
            w.write_all("const output = ".as_bytes())?;
            let source = lutra_bin::print_source(data, ty, ty_defs)?;
            w.write_all(source.as_bytes())?;
            w.write_all("\n".as_bytes())?;
        }

        DataFormat::Ltd => {
            let mut buf = lutra_bin::bytes::BytesMut::new();
            typed_data::encode(&mut buf, data, ty, ty_defs)?;
            w.write_all(&buf)?;
        }

        DataFormat::Csv => {
            let tabular = lutra_bin::TabularReader::new(data, ty, ty_defs);
            w.write_all(tabular.column_names().join(",").as_bytes())?;
            w.write_all("\n".as_bytes())?;
            for row in tabular {
                for (index, cell) in row.iter().enumerate() {
                    if index > 0 {
                        w.write_all(",".as_bytes())?;
                    }
                    let source = lutra_bin::print_source(cell.data(), cell.ty(), cell.ty_defs())?;
                    w.write_all(source.as_bytes())?;
                }
                w.write_all("\n".as_bytes())?;
            }
        }

        DataFormat::Parquet => {
            use parquet::arrow::ArrowWriter;

            let ty_item = ty.kind.as_array().unwrap(); // TODO
            let batch = lutra_arrow::lutra_to_arrow(data, ty_item, ty_defs)?;

            // ArrowWriter requires Seek, so write to an in-memory buffer first
            let mut buf = Vec::new();
            {
                let mut writer = ArrowWriter::try_new(&mut buf, batch.schema(), None)?;
                writer.write(&batch)?;
                writer.finish()?;
            }
            w.write_all(&buf)?;
        }

        DataFormat::Table => {
            let table = lutra_bin::Table::new(data, ty, ty_defs);
            let rendered = table.render();
            w.write_all(rendered.as_bytes())?;
        }
    }

    Ok(w.get_bytes_written())
}

pub fn read_input(
    input_path: Option<path::PathBuf>,
    cmd_format: Option<DataFormat>,
    ty: &rr::ProgramType,
) -> anyhow::Result<Vec<u8>> {
    let format = cmd_format
        // try to infer from input path extension
        .or_else(|| {
            input_path
                .as_ref()
                .and_then(|p| p.extension())
                .and_then(|ex| ex.to_str())
                .and_then(|ex| DataFormat::from_str(ex).ok())
        })
        // fallback to lt
        .unwrap_or(DataFormat::Lt);

    if let Some(input_path) = input_path {
        eprintln!("Reading input...");
        let mut reader = BufReader::new(std::fs::File::open(input_path)?);
        read_format(&mut reader, format, &ty.input, &ty.defs)
    } else {
        // don't allow reading from stdin (a common mistake)
        // we can implement it when it's needed
        if !ty.input.is_unit() {
            return Err(anyhow::anyhow!(
                "Missing --input. Expected type {}",
                lutra_bin::ir::print_ty(&ty.input)
            ));
        }

        Ok(Vec::new())
    }
}

fn read_format(
    r: &mut impl Read,
    format: DataFormat,
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> anyhow::Result<Vec<u8>> {
    match format {
        DataFormat::Ld => {
            let mut buf = Vec::new();
            r.read_to_end(&mut buf)?;
            Ok(buf)
        }

        DataFormat::Lt => {
            todo!()
        }

        DataFormat::Ltd => {
            let mut buf = Vec::new();
            r.read_to_end(&mut buf)?;
            Ok(buf.split_off(typed_data::data_offset()))
        }

        DataFormat::Csv => {
            todo!()
        }

        DataFormat::Parquet => {
            let mut buf = Vec::new();
            r.read_to_end(&mut buf)?;
            let buf = bytes::Bytes::from(buf);

            use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
            let builder = ParquetRecordBatchReaderBuilder::try_new(buf)?;
            let batches = builder.build()?.collect::<Result<_, _>>()?;

            let res = lutra_arrow::arrow_to_lutra(batches, ty, ty_defs)?;
            Ok(res.to_vec())
        }

        DataFormat::Table => Err(anyhow::anyhow!("Table format is not supported for input")),
    }
}

struct WriteCounter<W> {
    inner: W,
    bytes_written: usize,
}

impl<W: Write> WriteCounter<W> {
    fn new(inner: W) -> Self {
        Self {
            inner,
            bytes_written: 0,
        }
    }

    fn get_bytes_written(&self) -> usize {
        self.bytes_written
    }
}

impl<W: Write> Write for WriteCounter<W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let n = self.inner.write(buf)?;
        self.bytes_written += n;
        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}
