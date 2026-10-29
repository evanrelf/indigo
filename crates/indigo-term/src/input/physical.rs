//! Splitting bytes from the terminal into sequences, without interpreting them

use crate::input::{BRACKETED_PASTE_BEGIN, BRACKETED_PASTE_END};
use std::{cmp::min, ops::Deref};
use tinyvec::{ArrayVec, TinyVec};
use winnow::{
    Partial,
    combinator::{alt, repeat},
    error::{ErrMode, Needed},
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
    /// Start of pasted text, when bracketed paste is enabled
    ///
    /// <https://en.wikipedia.org/wiki/Bracketed-paste>
    PasteBegin,
    /// Some pasted text, between [`Token::PasteBegin`] and [`Token::PasteEnd`]
    PasteChunk(ArrayVec<[u8; 32]>),
    /// End of pasted text
    PasteEnd,
}

#[derive(Default)]
pub struct Tokenizer {
    buffer: ArrayVec<[u8; 1024]>,
    in_paste: bool,
}

impl Tokenizer {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add as many bytes as fit, returning how many were added. Take tokens with
    /// [`Tokenizer::next_token`] to make room for more.
    #[must_use]
    pub fn feed(&mut self, bytes: &[u8]) -> usize {
        let count = min(bytes.len(), self.buffer.capacity() - self.buffer.len());
        self.buffer.extend_from_slice(&bytes[..count]);
        count
    }

    /// Split off the next token, or return `None` if more bytes are needed.
    pub fn next_token(&mut self) -> Option<Token> {
        loop {
            // Pasted text isn't garbage
            if !self.in_paste {
                self.skip_garbage();
            }

            if self.buffer.is_empty() {
                return None;
            }

            let input = Partial::new(self.buffer.as_slice());

            let mut parser = if self.in_paste { paste_token } else { token };

            match parser.parse_peek(input) {
                Ok((remaining, token)) => {
                    let consumed = self.buffer.len() - remaining.len();
                    self.buffer.drain(..consumed);
                    match token {
                        Token::PasteBegin => self.in_paste = true,
                        Token::PasteEnd => self.in_paste = false,
                        _ => {}
                    }
                    return Some(token);
                }
                Err(ErrMode::Incomplete(_)) => return None,
                Err(_) => {
                    // Malformed sequence, skip the leading 0x1b and retry
                    self.buffer.drain(..1);
                }
            }
        }
    }

    /// Skip any non-ESC bytes at the front of the buffer. These are garbage bytes that don't start
    /// any recognized sequence.
    fn skip_garbage(&mut self) {
        let garbage_count = self
            .buffer
            .iter()
            .position(|&byte| byte == /* ESC */ 0x1B)
            .unwrap_or(self.buffer.len());
        if garbage_count > 0 {
            self.buffer.drain(..garbage_count);
        }
    }
}

fn token(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Token> {
    alt((paste_begin, csi, osc)).parse_next(input)
}

fn paste_token(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Token> {
    alt((paste_end, paste_chunk)).parse_next(input)
}

fn paste_begin(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Token> {
    BRACKETED_PASTE_BEGIN.void().parse_next(input)?;
    Ok(Token::PasteBegin)
}

fn paste_end(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Token> {
    BRACKETED_PASTE_END.void().parse_next(input)?;
    Ok(Token::PasteEnd)
}

fn paste_chunk(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Token> {
    if input.is_empty() {
        return Err(ErrMode::Incomplete(Needed::new(1)));
    }
    // Take whatever bytes are available, up to the chunk's capacity, but stop before the next ESC
    // in case it begins the end marker. A leading ESC is pasted text, since `paste_end` didn't
    // match it.
    let mut chunk = ArrayVec::new();
    let length = input
        .iter()
        .skip(1)
        .position(|&byte| byte == 0x1B)
        .map_or(input.len(), |position| position + 1);
    let length = min(length, chunk.capacity());
    chunk.extend_from_slice(input.next_slice(length));
    Ok(Token::PasteChunk(chunk))
}

fn csi(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Token> {
    "\x1b[".void().parse_next(input)?;
    let Accumulated(parameter_bytes) = repeat(0.., one_of(0x30..=0x3F)).parse_next(input)?;
    if parameter_bytes.is_heap() {
        log::debug!(
            "got {} csi parameter bytes, overflowing to heap",
            parameter_bytes.len()
        );
    }
    let Accumulated(intermediate_bytes) = repeat(0.., one_of(0x20..=0x2F)).parse_next(input)?;
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

fn osc(input: &mut Partial<&[u8]>) -> winnow::ModalResult<Token> {
    "\x1b]".void().parse_next(input)?;
    let Accumulated(data_bytes) = repeat(0.., one_of(0x20..=0xFF)).parse_next(input)?;
    if data_bytes.is_heap() {
        log::debug!(
            "found {} osc intermediate bytes, overflowing to heap",
            data_bytes.len()
        );
    }
    alt(("\x07", "\x1b\x5c")).void().parse_next(input)?;
    Ok(Token::Osc { data_bytes })
}

struct Accumulated<T: tinyvec::Array>(TinyVec<T>);

impl<T: tinyvec::Array> Deref for Accumulated<T> {
    type Target = TinyVec<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, const N: usize> Accumulate<T> for Accumulated<[T; N]>
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
