mod client;
mod server;

pub use client::Client;
pub use server::Server;

use tokio::io::{AsyncRead, AsyncReadExt, AsyncWrite, AsyncWriteExt};

async fn write_message(
    mut tx: impl AsyncWrite + Unpin,
    e: impl lutra_bin::Encode,
) -> std::io::Result<()> {
    tracing::trace!("write message");

    let buf = e.encode();

    tracing::trace!(".. len = {}", buf.len());
    tx.write_all(&(buf.len() as u32).to_le_bytes()).await?;
    tracing::trace!(".. contents");
    tx.write_all(&buf).await?;
    tracing::trace!("done");
    Ok(())
}

async fn read_message<D: lutra_bin::Decode + Sized>(
    mut rx: impl AsyncRead + Unpin,
) -> std::io::Result<D> {
    tracing::trace!("read message");

    let mut buf = [0; 4];
    rx.read_exact(&mut buf).await?;
    let len = u32::from_le_bytes(buf) as usize;

    tracing::trace!(".. len = {len}");
    let mut buf = vec![0; len];
    rx.read_exact(&mut buf).await?;

    tracing::trace!(".. decode");
    let r = D::decode(&buf).map_err(|e| {
        std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("Failed to decode message: {:?}", e),
        )
    })?;

    tracing::trace!(".. done");
    Ok(r)
}
