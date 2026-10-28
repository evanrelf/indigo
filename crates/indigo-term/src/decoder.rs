use crate::{event::Event, parser};
use std::cmp::min;
use tinyvec::ArrayVec;
use winnow::{Parser, Partial, error::ErrMode};

#[derive(Default)]
pub struct Decoder {
    buffer: ArrayVec<[u8; 1024]>,
}

impl Decoder {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Add as many bytes as fit, returning how many were added. Take events with
    /// [`Decoder::next_event`] to make room for more.
    #[must_use]
    pub fn feed(&mut self, bytes: &[u8]) -> usize {
        let count = min(bytes.len(), self.buffer.capacity() - self.buffer.len());
        self.buffer.extend_from_slice(&bytes[..count]);
        count
    }

    /// Decode the next event, or return `None` if more bytes are needed.
    pub fn next_event(&mut self) -> Option<Event> {
        loop {
            self.skip_garbage();

            if self.buffer.is_empty() {
                return None;
            }

            let input = Partial::new(self.buffer.as_slice());

            match parser::event.parse_peek(input) {
                Ok((remaining, event)) => {
                    let consumed = self.buffer.len() - remaining.len();
                    self.buffer.drain(..consumed);
                    return Some(event);
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
