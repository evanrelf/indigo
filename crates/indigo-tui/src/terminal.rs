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
};

// TODO: Fix bug where keyboard enhancements[1] linger after Indigo exists.
//
// Repro:
//   1. Open a file in your pager. Keys work as usual.
//   2. Launch and quit `indigo-tui`.
//   3. Open a file in your pager. Keys don't work correctly. `less` is frozen with a bell, `least`
//      interprets key up and down as separate events.
//
// May have something to do with `PopKeyboardEnhancementFlags` only popping one level? Not sure.
//
// [1]: https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement

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
        let _ = ratatui::try_restore();
        let mut stdout = io::stdout();
        let _ = stdout.queue(PopKeyboardEnhancementFlags);
        let _ = stdout.queue(DisableMouseCapture);
        let _ = stdout.flush();
    }
}
