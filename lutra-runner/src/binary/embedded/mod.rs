mod client;

pub use client::Client;

use embedded_io_async::{Read, ReadExactError, Write};

async fn write_message<W: Write + Unpin>(
    mut tx: W,
    e: impl lutra_bin::Encode,
) -> Result<(), W::Error> {
    tracing::debug!("writing message");
    let buf = e.encode();

    tracing::debug!(".. len = {}", buf.len());
    tx.write_all(&(buf.len() as u32).to_le_bytes()).await?;
    tracing::debug!(".. contents");
    tx.write_all(&buf).await?;
    tracing::debug!(".. done");
    Ok(())
}

async fn read_message<R, D>(mut rx: R) -> Result<lutra_bin::Result<D>, ReadExactError<R::Error>>
where
    R: Read + Unpin,
    D: lutra_bin::Decode + Sized,
{
    tracing::debug!("read message");

    let mut buf = [0; 4];
    rx.read_exact(&mut buf).await?;
    let len = u32::from_le_bytes(buf) as usize;
    tracing::debug!(".. len = {len}");

    let mut buf = vec![0; len];
    rx.read_exact(&mut buf).await?;
    tracing::debug!(".. decode");

    let r = D::decode(&buf);
    tracing::debug!(".. done");
    Ok(r)
}
