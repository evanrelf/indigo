use crossterm::{
    event::{
        DisableMouseCapture, EnableMouseCapture, KeyboardEnhancementFlags,
        PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
    },
    execute,
    terminal::{Clear, ClearType},
};
use std::{
    io,
    ops::{Deref, DerefMut},
    thread,
};

pub struct TerminalGuard(pub ratatui::DefaultTerminal);

#[must_use]
pub fn init() -> TerminalGuard {
    let terminal = ratatui::init();
    execute!(
        io::stdout(),
        Clear(ClearType::All),
        PushKeyboardEnhancementFlags(KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES),
        EnableMouseCapture,
    )
    .unwrap();
    TerminalGuard(terminal)
}

impl Deref for TerminalGuard {
    type Target = ratatui::DefaultTerminal;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TerminalGuard {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        if !thread::panicking() {
            ratatui::restore();
        }
        let _ = execute!(
            io::stdout(),
            PopKeyboardEnhancementFlags,
            DisableMouseCapture,
        );
    }
}
