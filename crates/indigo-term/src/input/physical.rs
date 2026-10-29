//! Splitting bytes from the terminal into sequences, without interpreting them

use std::ops::Deref;
use tinyvec::TinyVec;
use winnow::{
    Partial,
    combinator::{alt, repeat},
    prelude::*,
    stream::Accumulate,
    token::one_of,
};

#[derive(Debug, PartialEq)]
pub enum Token {
    /// Control Sequence Introducer
    ///
    /// <https://ghostty.org/docs/vt/concepts/sequences#csi>
    Csi {
        parameter_bytes: TinyVec<[u8; 32]>,
        intermediate_bytes: TinyVec<[u8; 2]>,
        final_byte: u8,
    },
    /// Operating System Command
    ///
    /// <https://ghostty.org/docs/vt/concepts/sequences#osc>
    Osc { data_bytes: TinyVec<[u8; 32]> },
}

pub(crate) fn csi(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Token> {
    "\x1b[".void().parse_next(input)?;
    let MyTinyVec(parameter_bytes) = repeat(0.., one_of(0x30..=0x3F)).parse_next(input)?;
    if parameter_bytes.is_heap() {
        log::debug!(
            "got {} csi parameter bytes, overflowing to heap",
            parameter_bytes.len()
        );
    }
    let MyTinyVec(intermediate_bytes) = repeat(0.., one_of(0x20..=0x2F)).parse_next(input)?;
    if intermediate_bytes.is_heap() {
        log::debug!(
            "got {} csi intermediate bytes, overflowing to heap",
            intermediate_bytes.len()
        );
    }
    let final_byte = one_of(0x40..=0x7E).parse_next(input)?;
    Ok(Token::Csi {
        parameter_bytes,
        intermediate_bytes,
        final_byte,
    })
}

pub(crate) fn osc(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Token> {
    "\x1b]".void().parse_next(input)?;
    let MyTinyVec(data_bytes) = repeat(0.., one_of(0x20..=0xFF)).parse_next(input)?;
    if data_bytes.is_heap() {
        log::debug!(
            "found {} osc intermediate bytes, overflowing to heap",
            data_bytes.len()
        );
    }
    alt(("\x07", "\x1b\x5c")).void().parse_next(input)?;
    Ok(Token::Osc { data_bytes })
}

struct MyTinyVec<T: tinyvec::Array>(TinyVec<T>);

impl<T: tinyvec::Array> Deref for MyTinyVec<T> {
    type Target = TinyVec<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, const N: usize> Accumulate<T> for MyTinyVec<[T; N]>
where
    T: Default,
{
    fn initial(capacity: Option<usize>) -> Self {
        if let Some(capacity) = capacity {
            Self(TinyVec::with_capacity(capacity))
        } else {
            Self(TinyVec::new())
        }
    }
    fn accumulate(&mut self, acc: T) {
        self.0.push(acc);
    }
}
