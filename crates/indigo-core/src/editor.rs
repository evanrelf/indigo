use crate::{buffer::Buffer, mode::Mode};
use camino::Utf8PathBuf;

#[derive(Default)]
pub struct Editor {
    pub buffer: Buffer,
    pub mode: Mode,
    pub terminal_height: usize,
    pub pwd: Option<Utf8PathBuf>,
    pub message: Option<Result<String, String>>,
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
