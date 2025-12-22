use std::path;
use std::pin::Pin;
use std::str::FromStr;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::task::{Context, Poll, ready};

use tokio::fs;
use tokio::io::AsyncWrite;
use tokio::io::{self, AsyncReadExt, AsyncWriteExt};

use lutra_bin::{bytes, ir, rr, typed_data};

use crate::DataFormat;

pub async fn write_output(
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
        let mut writer = io::BufWriter::new(fs::File::create(output_path).await?);

        let size = write_format(&mut writer, &data, format, &ty.output, &ty.defs).await?;
        writer.flush().await?;
        writer.shutdown().await?;
        eprintln!(
            "Output written to {} ({}, {size} bytes)",
            output_path.display(),
            format.as_ref(),
        );
    } else {
        // stdout
        let mut writer = io::BufWriter::new(io::stdout());

        write_format(&mut writer, &data, format, &ty.output, &ty.defs).await?;
        writer.flush().await?;
        writer.shutdown().await?;
    };

    Ok(())
}

async fn write_format(
    w: &mut (impl io::AsyncWrite + Unpin + Send),
    data: &[u8],
    format: DataFormat,
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> anyhow::Result<usize> {
    let mut w = AsyncWriteCounter::new(w);

    match format {
        DataFormat::Ld => w.write_all(data).await?,

        DataFormat::Lt => {
            w.write_all("const output = ".as_bytes()).await?;
            let source = lutra_bin::print_source(data, ty, ty_defs)?;
            w.write_all(source.as_bytes()).await?;
            w.write_all("\n".as_bytes()).await?;
        }

        DataFormat::Ltd => {
            let mut buf = lutra_bin::bytes::BytesMut::new();
            typed_data::encode(&mut buf, data, ty, ty_defs)?;
            w.write_all(&buf).await?
        }
        DataFormat::Csv => {
            let tabular = lutra_bin::Tabular::new(data, ty, ty_defs);
            w.write_all(tabular.column_names().join(",").as_bytes())
                .await?;
            w.write_all("\n".as_bytes()).await?;
            for row in tabular {
                for (index, cell) in row.iter().enumerate() {
                    if index > 0 {
                        w.write_all(",".as_bytes()).await?;
                    }
                    let source = lutra_bin::print_source(cell.data(), cell.ty(), cell.ty_defs())?;
                    w.write_all(source.as_bytes()).await?;
                }
                w.write_all("\n".as_bytes()).await?;
            }
        }
        DataFormat::Parquet => {
            use parquet::arrow::AsyncArrowWriter;

            let ty_item = ty.kind.as_array().unwrap(); // TODO

            let data = lutra_arrow::lutra_to_arrow(data, &ty_item);

            let mut builder = AsyncArrowWriter::try_new(&mut w, data.schema(), None).unwrap();
            builder.write(&data).await.unwrap();
            builder.finish().await.unwrap();
        }
    }
    Ok(w.get_bytes_written())
}

pub async fn read_input(
    input_path: Option<path::PathBuf>,
    cmd_format: Option<DataFormat>,
    ty: &rr::ProgramType,
) -> anyhow::Result<Vec<u8>> {
    let format = cmd_format
        // try to infer from output path extension
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
        let mut reader = io::BufReader::new(fs::File::open(input_path).await?);
        read_format(&mut reader, format, &ty.input, &ty.defs).await
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

async fn read_format(
    r: &mut (impl io::AsyncRead + io::AsyncSeek + Send + Unpin),
    format: DataFormat,
    ty: &ir::Ty,
    ty_defs: &[ir::TyDef],
) -> anyhow::Result<Vec<u8>> {
    match format {
        DataFormat::Ld => {
            let mut buf = Vec::new();
            r.read_to_end(&mut buf).await?;
            Ok(buf)
        }

        DataFormat::Lt => {
            todo!()
        }

        DataFormat::Ltd => {
            let mut buf = Vec::new();
            r.read_to_end(&mut buf).await?;

            Ok(buf.split_off(typed_data::data_offset()))
        }
        DataFormat::Csv => {
            todo!()
        }
        DataFormat::Parquet => {
            // read all to memory
            let mut buf = Vec::new();
            r.read_to_end(&mut buf).await?;
            let buf = bytes::Bytes::from(buf);

            // init parquet-to-arrow reader
            use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
            let builder = ParquetRecordBatchReaderBuilder::try_new(buf)?;
            let reader = builder.build()?;

            // convert arrow to lutra
            let res = lutra_arrow::arrow_to_lutra(reader, ty, ty_defs)
                .ok_or_else(|| anyhow::anyhow!("invalid parquet format for this type"))?;
            Ok(res.to_vec())
        }
    }
}

pub struct AsyncWriteCounter<W> {
    inner: W,
    bytes_written: AtomicUsize,
}

impl<W: AsyncWrite> AsyncWriteCounter<W> {
    pub fn new(inner: W) -> Self {
        Self {
            inner,
            bytes_written: AtomicUsize::new(0),
        }
    }

    pub fn get_bytes_written(&self) -> usize {
        self.bytes_written.load(Ordering::Relaxed)
    }
}

impl<W: AsyncWrite + Unpin> AsyncWrite for AsyncWriteCounter<W> {
    fn poll_write(
        mut self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        let bytes_written = ready!(Pin::new(&mut self.inner).poll_write(cx, buf))?;
        self.bytes_written
            .fetch_add(bytes_written, Ordering::Relaxed);
        Poll::Ready(Ok(bytes_written))
    }

    fn poll_flush(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Pin::new(&mut self.inner).poll_flush(cx)
    }

    fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<std::io::Result<()>> {
        Pin::new(&mut self.inner).poll_shutdown(cx)
    }
}
