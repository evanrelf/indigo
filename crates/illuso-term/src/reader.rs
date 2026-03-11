use crate::{event::Event, input};
use std::io::{self, Read};
use tinyvec::ArrayVec;
use winnow::{Parser, Partial, error::ErrMode};

#[derive(Default)]
pub struct Reader {
    buffer: ArrayVec<[u8; 1024]>,
}

impl Reader {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Read the next event from the input source. Blocks until a complete event is available.
    pub fn read_event(&mut self, input: &mut impl Read) -> io::Result<Event> {
        loop {
            self.skip_garbage();

            if !self.buffer.is_empty() {
                let input_slice = Partial::new(self.buffer.as_slice());
                match input::event.parse_peek(input_slice) {
                    Ok((remaining, event)) => {
                        let consumed = self.buffer.len() - remaining.len();
                        self.buffer.drain(..consumed);
                        return Ok(event);
                    }
                    Err(ErrMode::Incomplete(_)) => {
                        // Need more bytes, fall through to fill_buffer below
                    }
                    Err(_) => {
                        // Malformed sequence, skip the leading 0x1b and retry
                        self.buffer.drain(..1);
                        continue;
                    }
                }
            }

            self.fill_buffer(input)?;
        }
    }

    /// Read bytes from the input source into the internal buffer. Blocks until at least one byte is
    /// available.
    fn fill_buffer(&mut self, input: &mut impl Read) -> io::Result<()> {
        let remaining_capacity = self.buffer.capacity() - self.buffer.len();
        if remaining_capacity == 0 {
            return Err(io::Error::other("reader buffer full"));
        }
        let mut tmp = [0u8; 256];
        let max_read = remaining_capacity.min(tmp.len());
        let bytes_read = input.read(&mut tmp[..max_read])?;
        if bytes_read == 0 {
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "input closed"));
        }
        self.buffer.extend_from_slice(&tmp[..bytes_read]);
        Ok(())
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
