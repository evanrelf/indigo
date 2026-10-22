use crate::{
    buffer::BufferKey,
    key::Key,
    mode::Mode,
    window::{WindowKey, WindowState},
};
use slotmap::SlotMap;
use std::{num::NonZeroUsize, process::ExitCode};

#[derive(Clone)]
pub struct ClientState {
    pub(crate) windows: SlotMap<WindowKey, WindowState>,
    pub(crate) focused_window: WindowKey,
    pub mode: Mode,
    pub count: Option<NonZeroUsize>,
    pub pending_keys: Vec<Key>,
    pub message: Option<Result<String, String>>,
    exit_code: Option<ExitCode>,
}

impl ClientState {
    #[must_use]
    pub(crate) fn new(buffer_key: BufferKey) -> Self {
        let window = WindowState::new(buffer_key);
        let mut windows = SlotMap::default();
        let window_key = windows.insert(window);

        Self {
            windows,
            focused_window: window_key,
            mode: Mode::default(),
            count: None,
            pending_keys: Vec::new(),
            message: None,
            exit_code: None,
        }
    }

    #[must_use]
    pub fn exit_code(&self) -> Option<ExitCode> {
        self.exit_code
    }

    /// Doesn't actually terminate the process, just records the desired exit code.
    pub fn exit(&mut self, exit_code: u8) {
        self.exit_code = Some(ExitCode::from(exit_code));
    }
}
