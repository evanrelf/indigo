use crate::{
    buffer::Buffer,
    io::{Io, NoIo},
    mode::Mode,
    window::Window,
};
use camino::Utf8PathBuf;
use std::process::ExitCode;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from buffer")]
    Buffer(#[source] anyhow::Error),
}

pub struct Editor {
    pub io: Box<dyn Io>,
    pub buffer: Buffer,
    pub window: Window,
    pub mode: Mode,
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
    pub(crate) fn assert_invariants(&self) -> anyhow::Result<()> {
        self.buffer.assert_invariants().map_err(Error::Buffer)?;
        Ok(())
    }
}

impl Default for Editor {
    fn default() -> Self {
        Self {
            io: Box::new(NoIo),
            buffer: Buffer::default(),
            window: Window::default(),
            mode: Mode::default(),
            pwd: None,
            message: None,
            exit: None,
        }
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
