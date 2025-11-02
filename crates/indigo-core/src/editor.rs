use crate::{
    buffer::{self, Buffer},
    mode::Mode,
};
use camino::Utf8PathBuf;
use std::process::ExitCode;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from buffer")]
    Buffer(#[source] buffer::Error),
}

#[derive(Default)]
pub struct Editor {
    pub buffer: Buffer,
    pub mode: Mode,
    pub terminal_height: usize,
    pub pwd: Option<Utf8PathBuf>,
    pub message: Option<Result<String, String>>,
    pub exit: Option<ExitCode>,
}

impl Editor {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[expect(dead_code)]
    pub(crate) fn assert_invariants(&self) -> Result<(), Error> {
        self.buffer.assert_invariants().map_err(Error::Buffer)?;
        Ok(())
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
