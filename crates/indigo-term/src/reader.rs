use crate::{decoder::Decoder, event::Event};
use std::io::{self, Read};
use tinyvec::ArrayVec;

#[derive(Default)]
pub struct Reader {
    decoder: Decoder,
    unfed: ArrayVec<[u8; 256]>,
}

impl Reader {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Read the next event from the input source. Blocks until a complete event is available.
    pub fn read_event(&mut self, input: &mut impl Read) -> io::Result<Event> {
        loop {
            if let Some(event) = self.decoder.next_event() {
                return Ok(event);
            }

            if self.unfed.is_empty() {
                let bytes_read = input.read(self.unfed.grab_spare_slice_mut())?;
                if bytes_read == 0 {
                    return Err(io::Error::new(io::ErrorKind::UnexpectedEof, "input closed"));
                }
                self.unfed.set_len(bytes_read);
            }

            let bytes_fed = self.decoder.feed(&self.unfed);
            if bytes_fed == 0 {
                return Err(io::Error::other("reader buffer full"));
            }
            self.unfed.drain(..bytes_fed);
        }
    }
}
