pub trait Encode {
    fn encode(&self, w: &mut impl std::io::Write) -> std::io::Result<()>;
}

pub trait Decode: Sized {
    fn decode(r: &mut impl std::io::Read) -> std::io::Result<Self>;
}

impl Encode for i64 {
    fn encode(&self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        w.write_all(&self.to_le_bytes())
    }
}
impl Encode for f64 {
    fn encode(&self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        w.write_all(&self.to_le_bytes())
    }
}
impl Encode for bool {
    fn encode(&self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        w.write_all(&[(*self as u8)])
    }
}
impl Encode for String {
    fn encode(&self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        w.write_all(&(self.len() as u64).to_le_bytes())?;
        w.write_all(self.as_bytes())
    }
}
impl<E: Encode> Encode for Vec<E> {
    fn encode(&self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        w.write_all(&(self.len() as u64).to_le_bytes())?;
        for i in self {
            i.encode(w)?;
        }
        Ok(())
    }
}

impl Decode for i64 {
    fn decode(r: &mut impl std::io::Read) -> std::io::Result<Self> {
        let bytes = read_bytes::<8>(r)?;
        Ok(i64::from_le_bytes(bytes))
    }
}
impl Decode for f64 {
    fn decode(r: &mut impl std::io::Read) -> std::io::Result<Self> {
        let bytes = read_bytes::<8>(r)?;
        Ok(f64::from_le_bytes(bytes))
    }
}
impl Decode for bool {
    fn decode(r: &mut impl std::io::Read) -> std::io::Result<Self> {
        let [v] = read_bytes::<1>(r)?;
        Ok(v != 0)
    }
}
impl Decode for String {
    fn decode(r: &mut impl std::io::Read) -> std::io::Result<Self> {
        let bytes = read_bytes::<8>(r)?;
        let len = u64::from_le_bytes(bytes) as usize;

        let mut buf = Vec::with_capacity(len);
        buf.resize(len, 0);
        r.read_exact(&mut buf)?;

        Ok(String::from_utf8(buf).unwrap())
    }
}
impl<E: Decode> Decode for Vec<E> {
    fn decode(r: &mut impl std::io::Read) -> std::io::Result<Self> {
        let bytes = read_bytes::<8>(r)?;
        let len = u64::from_le_bytes(bytes) as usize;

        let mut buf = Vec::with_capacity(len);
        for _ in 0..len {
            buf.push(E::decode(r)?);
        }

        Ok(buf)
    }
}

fn read_bytes<const N: usize>(r: &mut impl std::io::Read) -> std::io::Result<[u8; N]> {
    let mut buf = [0_u8; N];
    r.read_exact(&mut buf)?;
    Ok(buf)
}
