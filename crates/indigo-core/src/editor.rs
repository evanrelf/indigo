use crate::{
    buffer::Buffer,
    fs::{Fs, NoFs},
    mode::Mode,
    settings::Settings,
    window::{Window, WindowMut, WindowState},
};
use camino::Utf8PathBuf;
use std::{process::ExitCode, rc::Rc};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from buffer")]
    Buffer(#[source] anyhow::Error),
}

pub struct Editor {
    pub fs: Rc<dyn Fs>,
    buffer: Buffer,
    window: WindowState,
    pub mode: Mode,
    pub pwd: Option<Utf8PathBuf>,
    pub message: Option<Result<String, String>>,
    pub settings: Settings,
    exit_code: Option<ExitCode>,
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

    #[must_use]
    pub fn exit_code(&self) -> Option<ExitCode> {
        self.exit_code
    }

    /// Doesn't actually terminate the process, just records the desired exit code.
    pub fn exit(&mut self, exit_code: u8) {
        self.exit_code = Some(ExitCode::from(exit_code));
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
            fs: Rc::new(NoFs),
            buffer: Buffer::default(),
            window: WindowState::default(),
            mode: Mode::default(),
            pwd: None,
            message: None,
            settings: Settings::default(),
            exit_code: None,
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
