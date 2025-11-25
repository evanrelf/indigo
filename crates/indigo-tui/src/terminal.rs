use ratatui::crossterm::{
    QueueableCommand as _,
    event::{
        DisableMouseCapture, EnableMouseCapture, KeyboardEnhancementFlags,
        PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
    },
    terminal::{Clear, ClearType, supports_keyboard_enhancement},
};
use std::{
    io::{self, Write as _},
    ops::{Deref, DerefMut},
    thread,
};

pub struct TerminalGuard(pub ratatui::DefaultTerminal);

pub fn init() -> anyhow::Result<TerminalGuard> {
    let terminal = ratatui::init();
    let mut stdout = io::stdout();
    stdout.queue(Clear(ClearType::All))?;
    stdout.queue(EnableMouseCapture)?;
    if let Ok(true) = supports_keyboard_enhancement() {
        stdout.queue(PushKeyboardEnhancementFlags(
            KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
                | KeyboardEnhancementFlags::REPORT_ALL_KEYS_AS_ESCAPE_CODES
                | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS
                | KeyboardEnhancementFlags::REPORT_EVENT_TYPES,
        ))?;
    }
    stdout.flush()?;
    Ok(TerminalGuard(terminal))
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
        let mut stdout = io::stdout();
        let _ = stdout.queue(PopKeyboardEnhancementFlags);
        let _ = stdout.queue(DisableMouseCapture);
        let _ = stdout.flush();
    }
}
