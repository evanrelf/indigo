use ratatui::crossterm::{
    event::{
        DisableMouseCapture, EnableMouseCapture, KeyboardEnhancementFlags,
        PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
    },
    execute,
    terminal::{Clear, ClearType, supports_keyboard_enhancement},
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
    execute!(io::stdout(), Clear(ClearType::All), EnableMouseCapture).unwrap();
    if let Ok(true) = supports_keyboard_enhancement() {
        execute!(
            io::stdout(),
            PushKeyboardEnhancementFlags(
                KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
                    | KeyboardEnhancementFlags::REPORT_ALL_KEYS_AS_ESCAPE_CODES
                    | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS
                    | KeyboardEnhancementFlags::REPORT_EVENT_TYPES
            )
        )
        .unwrap();
    }
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
