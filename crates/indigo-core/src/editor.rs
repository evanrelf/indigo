use crate::{
    buffer::{Buffer, BufferKey},
    fs::{Fs, NoFs},
    key::Key,
    mode::{Mode, insert, normal, prompt, seek},
    selection::Selection,
    window::{Window, WindowKey, WindowMut, WindowState},
};
use camino::Utf8PathBuf;
use slotmap::SlotMap;
use std::{num::NonZeroUsize, process::ExitCode, sync::Arc};
use thiserror::Error;

#[cfg(any(feature = "arbitrary", test))]
use arbitrary::Arbitrary;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Error from buffer")]
    Buffer(#[source] anyhow::Error),
}

#[derive(Clone)]
pub struct Editor {
    pub fs: Arc<dyn Fs + Send + Sync>,
    buffers: SlotMap<BufferKey, Buffer>,
    windows: SlotMap<WindowKey, WindowState>,
    focused_window: WindowKey,
    pub mode: Mode,
    pub count: Option<NonZeroUsize>,
    pub pending_keys: Vec<Key>,
    pub pwd: Option<Utf8PathBuf>,
    pub message: Option<Result<String, String>>,
    exit_code: Option<ExitCode>,
}

impl Editor {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    // TODO: Figure out a safer/better way to add and remove buffers
    pub(crate) fn insert_buffer(&mut self, buffer: Buffer) -> BufferKey {
        self.buffers.insert(buffer)
    }

    #[must_use]
    pub fn get_buffer(&self, buffer_key: BufferKey) -> Option<&Buffer> {
        self.buffers.get(buffer_key)
    }

    #[must_use]
    pub fn get_buffer_mut(&mut self, buffer_key: BufferKey) -> Option<&mut Buffer> {
        self.buffers.get_mut(buffer_key)
    }

    #[must_use]
    pub fn focused_buffer(&self) -> &Buffer {
        let window = self
            .windows
            .get(self.focused_window)
            .expect("Window state is always kept valid");
        self.buffers
            .get(window.buffer)
            .expect("Window state is always kept valid")
    }

    #[must_use]
    pub fn focused_buffer_mut(&mut self) -> &mut Buffer {
        let window = self
            .windows
            .get_mut(self.focused_window)
            .expect("Window state is always kept valid");
        self.buffers
            .get_mut(window.buffer)
            .expect("Window state is always kept valid")
    }

    pub fn buffers(&self) -> impl Iterator<Item = (BufferKey, &Buffer)> {
        self.buffers.iter()
    }

    pub fn buffers_mut(&mut self) -> impl Iterator<Item = (BufferKey, &mut Buffer)> {
        self.buffers.iter_mut()
    }

    // TODO: Figure out a safer/better way to add and remove windows
    pub(crate) fn insert_window(&mut self, window: WindowState) -> WindowKey {
        assert!(
            self.buffers.contains_key(window.buffer),
            "Window has invalid buffer key"
        );
        self.windows.insert(window)
    }

    pub fn focus_window(&mut self, window_key: WindowKey) -> bool {
        if self.windows.contains_key(window_key) {
            self.focused_window = window_key;
            true
        } else {
            false
        }
    }

    #[must_use]
    pub fn get_window(&self, window_key: WindowKey) -> Option<Window<'_>> {
        let window = self.windows.get(window_key)?;
        let buffer = self
            .buffers
            .get(window.buffer)
            .expect("Window state is always kept valid");
        Some(Window::new(buffer, window))
    }

    #[must_use]
    pub fn get_window_mut(&mut self, window_key: WindowKey) -> Option<WindowMut<'_>> {
        let window = self.windows.get_mut(window_key)?;
        let buffer = self
            .buffers
            .get_mut(window.buffer)
            .expect("Window state is always kept valid");
        Some(WindowMut::new(buffer, window))
    }

    pub fn focused_window(&self) -> Window<'_> {
        let window = self
            .windows
            .get(self.focused_window)
            .expect("Window state is always kept valid");
        let buffer = self
            .buffers
            .get(window.buffer)
            .expect("Window state is always kept valid");
        Window::new(buffer, window)
    }

    pub fn focused_window_mut(&mut self) -> WindowMut<'_> {
        let window = self
            .windows
            .get_mut(self.focused_window)
            .expect("Window state is always kept valid");
        let buffer = self
            .buffers
            .get_mut(window.buffer)
            .expect("Window state is always kept valid");
        WindowMut::new(buffer, window)
    }

    // TODO: Add windows iterator

    #[must_use]
    pub fn exit_code(&self) -> Option<ExitCode> {
        self.exit_code
    }

    /// Doesn't actually terminate the process, just records the desired exit code.
    pub fn exit(&mut self, exit_code: u8) {
        self.exit_code = Some(ExitCode::from(exit_code));
    }

    pub fn handle_event(&mut self, mut event: Event) -> anyhow::Result<bool> {
        #[expect(irrefutable_let_patterns)]
        if let Event::Key(KeyEvent { key, .. }) = &mut event {
            key.normalize();
            self.pending_keys.push(*key);
        }

        let handled = match event {
            Event::Key(_) => match self.mode {
                Mode::Normal => normal::handle_keys(self),
                Mode::Seek(_) => seek::handle_keys(self),
                Mode::Insert => insert::handle_keys(self),
                Mode::Prompt(_) => prompt::handle_keys(self),
            },
        };

        Ok(handled)
    }

    #[expect(dead_code)]
    pub(crate) fn assert_invariants(&self) -> anyhow::Result<()> {
        for (_, window) in &self.windows {
            let buffer = self
                .buffers
                .get(window.buffer)
                .expect("Window state is always kept valid");
            let _selection =
                Selection::new(&buffer.text, &window.selection).map_err(Error::Buffer)?;
        }
        Ok(())
    }
}

impl Default for Editor {
    fn default() -> Self {
        let mut buffers = SlotMap::default();
        let buffer = Buffer::default();
        let buffer_key = buffers.insert(buffer);

        let window = WindowState::new(buffer_key, &buffers[buffer_key]);
        let mut windows = SlotMap::default();
        let window_key = windows.insert(window);

        Self {
            fs: Arc::new(NoFs),
            buffers,
            windows,
            focused_window: window_key,
            mode: Mode::default(),
            count: None,
            pending_keys: Vec::new(),
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

        let window = WindowState::new(buffer_key, &buffers[buffer_key]);
        let mut windows = SlotMap::default();
        let window_key = windows.insert(window);

        Self {
            buffers,
            windows,
            focused_window: window_key,
            ..Self::default()
        }
    }
}

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug)]
pub enum Event {
    Key(KeyEvent),
}

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug, PartialEq)]
pub struct KeyEvent {
    pub key: Key,
    pub kind: KeyEventKind,
}

#[cfg_attr(any(feature = "arbitrary", test), derive(Arbitrary))]
#[derive(Debug, PartialEq)]
pub enum KeyEventKind {
    Press,
    Repeat,
    Release,
}
