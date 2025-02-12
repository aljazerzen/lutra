#![cfg_attr(not(feature = "std"), no_std)]

mod client;

pub mod messages {
    include!(concat!(env!("OUT_DIR"), "/messages.rs"));
}

pub use client::Connection;

#[cfg(not(feature = "std"))]
extern crate alloc;

#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(feature = "std")]
use std::vec;

use embedded_io_async::{Read, ReadExactError, Write};

async fn write_message<W: Write + Unpin>(
    mut tx: W,
    e: impl lutra_bin::Encode,
) -> Result<(), W::Error> {
    log::debug!("writing message");
    let mut buf = lutra_bin::bytes::BytesMut::new();
    e.encode(&mut buf);

    log::debug!(".. len = {}", buf.len());
    tx.write_all(&(buf.len() as u32).to_le_bytes()).await?;
    log::debug!(".. contents");
    tx.write_all(&buf).await?;
    log::debug!(".. done");
    Ok(())
}

async fn read_message<R, D>(mut rx: R) -> Result<lutra_bin::Result<D>, ReadExactError<R::Error>>
where
    R: Read + Unpin,
    D: lutra_bin::Decode + Sized,
{
    log::debug!("read message");

    let mut buf = [0; 4];
    rx.read_exact(&mut buf).await?;
    let len = u32::from_le_bytes(buf) as usize;
    log::debug!(".. len = {len}");

    let mut buf = vec![0; len];
    rx.read_exact(&mut buf).await?;
    log::debug!(".. decode");

    let r = D::decode(&buf);
    log::debug!(".. done");
    Ok(r)
}
