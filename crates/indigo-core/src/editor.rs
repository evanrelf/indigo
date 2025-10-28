use crate::{buffer::Buffer, mode::Mode};

#[derive(Default)]
pub struct Editor {
    pub buffer: Buffer,
    pub mode: Mode,
    pub terminal_height: usize,
    pub exit: Option<u8>,
}

impl Editor {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

impl From<Buffer> for Editor {
    fn from(buffer: Buffer) -> Self {
        Self {
            buffer,
            ..Self::default()
        }
    }
}
