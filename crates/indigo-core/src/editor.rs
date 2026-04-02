use crate::{
    buffer::{Buffer, BufferKey},
    fs::{Fs, NoFs},
    mode::Mode,
    window::{Window, WindowMut, WindowState},
};
use camino::Utf8PathBuf;
use slotmap::SlotMap;
use std::{process::ExitCode, sync::Arc};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from buffer")]
    Buffer(#[source] anyhow::Error),
}

#[derive(Clone)]
pub struct Editor {
    pub fs: Arc<dyn Fs + Send + Sync>,
    buffers: SlotMap<BufferKey, Buffer>,
    window: WindowState,
    pub mode: Mode,
    pub pwd: Option<Utf8PathBuf>,
    pub message: Option<Result<String, String>>,
    exit_code: Option<ExitCode>,
}

impl Editor {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn window(&self) -> Window<'_> {
        let buffer = self
            .buffers
            .get(self.window.buffer)
            .expect("Window state is always kept valid");
        Window::new(buffer, &self.window)
    }

    pub fn window_mut(&mut self) -> WindowMut<'_> {
        let buffer = self
            .buffers
            .get_mut(self.window.buffer)
            .expect("Window state is always kept valid");
        WindowMut::new(buffer, &mut self.window)
    }

    // TODO: Add `get_buffer`, `get_focused_buffer`, all the buffers iterator, etc

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
        for buffer in self.buffers.values() {
            buffer.assert_invariants().map_err(Error::Buffer)?;
        }
        Ok(())
    }
}

impl Default for Editor {
    fn default() -> Self {
        let mut buffers = SlotMap::default();
        let buffer_key = buffers.insert(Buffer::default());
        let window = WindowState::new(buffer_key);
        Self {
            fs: Arc::new(NoFs),
            buffers,
            window,
            mode: Mode::default(),
            pwd: None,
            message: None,
            exit_code: None,
        }
    }
}

impl From<Buffer> for Editor {
    fn from(buffer: Buffer) -> Self {
        let mut buffers = SlotMap::default();
        let buffer_key = buffers.insert(buffer);
        let window = WindowState::new(buffer_key);
        Self {
            buffers,
            window,
            ..Self::default()
        }
    }
}
