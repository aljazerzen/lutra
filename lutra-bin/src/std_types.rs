use crate::{Decode, Encode, Layout, Result};

/// Standard-library types used by generated Lutra code.
pub mod ops {
    /// Result of a three-way comparison.
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    #[allow(non_camel_case_types)]
    pub enum Ordering {
        Less,
        Equal,
        Greater,
    }
}

/// An instant in time — microseconds since 1970-01-01T00:00:00 UTC.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Timestamp {
    pub microseconds: i64,
}

/// A calendar date — days since 1970-01-01.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Date {
    pub days_epoch: i32,
}

/// A time-of-day offset — microseconds since midnight (always in `[0, 86_400_000_000)`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Time {
    pub micros_midnight: u64,
}

/// A signed, unbounded duration — microseconds.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Duration {
    pub microseconds: i64,
}

impl core::ops::Add for Duration {
    type Output = Duration;

    fn add(self, rhs: Duration) -> Duration {
        Duration {
            microseconds: self.microseconds + rhs.microseconds,
        }
    }
}

impl core::ops::Sub for Duration {
    type Output = Duration;

    fn sub(self, rhs: Duration) -> Duration {
        Duration {
            microseconds: self.microseconds - rhs.microseconds,
        }
    }
}

impl core::ops::Neg for Duration {
    type Output = Duration;

    fn neg(self) -> Duration {
        Duration {
            microseconds: -self.microseconds,
        }
    }
}

/// A fixed-point decimal (scale = 2).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Decimal(pub i64);

impl Encode for Timestamp {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut crate::bytes::BytesMut) {
        self.microseconds.encode_head(buf)
    }

    fn encode_body(&self, _: (), _: &mut crate::bytes::BytesMut) {}
}

impl Layout for Timestamp {
    fn head_size() -> usize {
        i64::head_size()
    }
}

impl Decode for Timestamp {
    fn decode(buf: &[u8]) -> Result<Self> {
        Ok(Self {
            microseconds: i64::decode(buf)?,
        })
    }
}
impl Encode for Date {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut crate::bytes::BytesMut) {
        self.days_epoch.encode_head(buf)
    }

    fn encode_body(&self, _: (), _: &mut crate::bytes::BytesMut) {}
}
impl Layout for Date {
    fn head_size() -> usize {
        i32::head_size()
    }
}
impl Decode for Date {
    fn decode(buf: &[u8]) -> Result<Self> {
        Ok(Self {
            days_epoch: i32::decode(buf)?,
        })
    }
}
impl Encode for Time {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut crate::bytes::BytesMut) {
        self.micros_midnight.encode_head(buf)
    }

    fn encode_body(&self, _: (), _: &mut crate::bytes::BytesMut) {}
}
impl Layout for Time {
    fn head_size() -> usize {
        u64::head_size()
    }
}
impl Decode for Time {
    fn decode(buf: &[u8]) -> Result<Self> {
        Ok(Self {
            micros_midnight: u64::decode(buf)?,
        })
    }
}
impl Encode for Duration {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut crate::bytes::BytesMut) {
        self.microseconds.encode_head(buf)
    }

    fn encode_body(&self, _: (), _: &mut crate::bytes::BytesMut) {}
}
impl Layout for Duration {
    fn head_size() -> usize {
        i64::head_size()
    }
}
impl Decode for Duration {
    fn decode(buf: &[u8]) -> Result<Self> {
        Ok(Self {
            microseconds: i64::decode(buf)?,
        })
    }
}
impl Encode for Decimal {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut crate::bytes::BytesMut) {
        self.0.encode_head(buf)
    }

    fn encode_body(&self, _: (), _: &mut crate::bytes::BytesMut) {}
}
impl Layout for Decimal {
    fn head_size() -> usize {
        i64::head_size()
    }
}
impl Decode for Decimal {
    fn decode(buf: &[u8]) -> Result<Self> {
        Ok(Self(i64::decode(buf)?))
    }
}
impl Encode for ops::Ordering {
    type HeadPtr = ();

    fn encode_head(&self, buf: &mut crate::bytes::BytesMut) {
        let tag = match self {
            Self::Less => 0_u8,
            Self::Equal => 1,
            Self::Greater => 2,
        };
        tag.encode_head(buf)
    }

    fn encode_body(&self, _: (), _: &mut crate::bytes::BytesMut) {}
}
impl Layout for ops::Ordering {
    fn head_size() -> usize {
        8
    }
}
impl Decode for ops::Ordering {
    fn decode(buf: &[u8]) -> Result<Self> {
        Ok(match u8::decode(buf)? {
            0 => Self::Less,
            1 => Self::Equal,
            2 => Self::Greater,
            _ => return Err(crate::Error::InvalidData),
        })
    }
}

#[cfg(feature = "chrono")]
mod chrono_impls {
    use chrono::{Datelike, Timelike};

    use crate::std::Duration;

    use super::{Date, Time, Timestamp};

    const EPOCH_DAYS_FROM_CE: i32 = 719_163;
    const MICROS_PER_SECOND: u32 = 1_000_000;
    const MICROS_PER_DAY: i64 = 86_400_000_000;

    impl TryFrom<Timestamp> for chrono::DateTime<chrono::Utc> {
        type Error = crate::Error;

        fn try_from(value: Timestamp) -> core::result::Result<Self, Self::Error> {
            chrono::DateTime::from_timestamp_micros(value.microseconds)
                .ok_or(crate::Error::InvalidData)
        }
    }

    impl From<chrono::DateTime<chrono::Utc>> for Timestamp {
        fn from(value: chrono::DateTime<chrono::Utc>) -> Self {
            Self {
                microseconds: value.timestamp_micros(),
            }
        }
    }

    impl TryFrom<Date> for chrono::NaiveDate {
        type Error = crate::Error;

        fn try_from(value: Date) -> core::result::Result<Self, Self::Error> {
            chrono::NaiveDate::from_num_days_from_ce_opt(value.days_epoch + EPOCH_DAYS_FROM_CE)
                .ok_or(crate::Error::InvalidData)
        }
    }

    impl TryFrom<chrono::NaiveDate> for Date {
        type Error = crate::Error;

        fn try_from(value: chrono::NaiveDate) -> core::result::Result<Self, Self::Error> {
            let days = value.num_days_from_ce() - EPOCH_DAYS_FROM_CE;
            Ok(Self { days_epoch: days })
        }
    }

    impl TryFrom<Time> for chrono::NaiveTime {
        type Error = crate::Error;

        fn try_from(value: Time) -> core::result::Result<Self, Self::Error> {
            if value.micros_midnight >= MICROS_PER_DAY as u64 {
                return Err(crate::Error::InvalidData);
            }

            let secs = u32::try_from(value.micros_midnight / u64::from(MICROS_PER_SECOND))
                .map_err(|_| crate::Error::InvalidData)?;
            let micros = u32::try_from(value.micros_midnight % u64::from(MICROS_PER_SECOND))
                .map_err(|_| crate::Error::InvalidData)?;

            chrono::NaiveTime::from_num_seconds_from_midnight_opt(secs, micros * 1_000)
                .ok_or(crate::Error::InvalidData)
        }
    }

    impl From<chrono::NaiveTime> for Time {
        fn from(value: chrono::NaiveTime) -> Self {
            let secs = u64::from(value.num_seconds_from_midnight());
            let micros = u64::from(value.nanosecond() / 1_000);
            Self {
                micros_midnight: secs * u64::from(MICROS_PER_SECOND) + micros,
            }
        }
    }
    impl From<Duration> for chrono::Duration {
        fn from(value: Duration) -> Self {
            chrono::Duration::microseconds(value.microseconds)
        }
    }
    impl TryFrom<chrono::Duration> for Duration {
        type Error = crate::Error;

        fn try_from(value: chrono::Duration) -> core::result::Result<Self, Self::Error> {
            value
                .num_microseconds()
                .map(|microseconds| Duration { microseconds })
                .ok_or(crate::Error::InvalidData)
        }
    }
}
