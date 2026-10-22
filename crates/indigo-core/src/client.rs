use crate::{key::Key, mode::Mode};
use std::{num::NonZeroUsize, process::ExitCode};

#[derive(Clone, Default)]
pub struct Client {
    pub mode: Mode,
    pub count: Option<NonZeroUsize>,
    pub pending_keys: Vec<Key>,
    pub message: Option<Result<String, String>>,
    exit_code: Option<ExitCode>,
}

impl Client {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
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
