use ratatui::{
    Terminal,
    crossterm::{
        QueueableCommand as _,
        event::{
            DisableBracketedPaste, DisableMouseCapture, EnableBracketedPaste, EnableMouseCapture,
            KeyboardEnhancementFlags, PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
        },
        terminal::{
            Clear, ClearType, EnterAlternateScreen, enable_raw_mode, supports_keyboard_enhancement,
        },
    },
    prelude::CrosstermBackend,
};
use std::{
    io::{self, Write as _},
    ops::{Deref, DerefMut},
    panic,
};

pub struct TerminalGuard(pub ratatui::DefaultTerminal);

pub fn init() -> anyhow::Result<TerminalGuard> {
    let hook = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        ratatui::restore();
        hook(info);
    }));
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    stdout.queue(EnterAlternateScreen)?;
    stdout.queue(Clear(ClearType::All))?;
    stdout.queue(EnableMouseCapture)?;
    stdout.queue(EnableBracketedPaste)?;
    if let Ok(true) = supports_keyboard_enhancement() {
        stdout.queue(PushKeyboardEnhancementFlags(
            KeyboardEnhancementFlags::DISAMBIGUATE_ESCAPE_CODES
                | KeyboardEnhancementFlags::REPORT_ALL_KEYS_AS_ESCAPE_CODES
                | KeyboardEnhancementFlags::REPORT_ALTERNATE_KEYS
                | KeyboardEnhancementFlags::REPORT_EVENT_TYPES,
        ))?;
    }
    stdout.flush()?;
    let terminal = Terminal::new(CrosstermBackend::new(io::stdout()))?;
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
        let mut stdout = io::stdout();
        let _ = stdout.queue(PopKeyboardEnhancementFlags);
        let _ = stdout.queue(DisableBracketedPaste);
        let _ = stdout.queue(DisableMouseCapture);
        let _ = stdout.flush();
        let _ = ratatui::try_restore();
    }
}
