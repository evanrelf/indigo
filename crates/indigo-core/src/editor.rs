use crate::{
    buffer::Buffer,
    fs::{Fs, NoFs},
    mode::Mode,
    window::{Window, WindowMut, WindowState},
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
    pub fs: Box<dyn Fs>,
    pub buffer: Buffer,
    window: WindowState,
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

    pub fn window(&self) -> Window<'_> {
        Window::new(&self.buffer, &self.window)
    }

    pub fn window_mut(&mut self) -> WindowMut<'_> {
        WindowMut::new(&mut self.buffer, &mut self.window)
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
            fs: Box::new(NoFs),
            buffer: Buffer::default(),
            window: WindowState::default(),
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
