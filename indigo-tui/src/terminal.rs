use anyhow::Context as _;
use crossterm::{cursor, event, terminal};
use ratatui::prelude::{CrosstermBackend, Terminal};
use std::{
    io::Stdout,
    ops::{Deref, DerefMut},
    panic,
};

pub struct TerminalGuard {
    terminal: Terminal<CrosstermBackend<Stdout>>,
}

impl Deref for TerminalGuard {
    type Target = Terminal<CrosstermBackend<Stdout>>;

    fn deref(&self) -> &Self::Target {
        &self.terminal
    }
}

impl DerefMut for TerminalGuard {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.terminal
    }
}

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        exit()
            .context("Failed to exit terminal during drop")
            .unwrap();
    }
}

pub fn enter() -> anyhow::Result<TerminalGuard> {
    use event::KeyboardEnhancementFlags as KEF;

    let mut stdout = std::io::stdout();

    terminal::enable_raw_mode().context("Failed to enable raw mode")?;

    crossterm::execute!(
        stdout,
        terminal::EnterAlternateScreen,
        cursor::Hide,
        event::EnableMouseCapture,
        event::EnableBracketedPaste,
        event::PushKeyboardEnhancementFlags(
            KEF::DISAMBIGUATE_ESCAPE_CODES
                | KEF::REPORT_EVENT_TYPES
                | KEF::REPORT_ALL_KEYS_AS_ESCAPE_CODES
        ),
    )
    .context("Failed to execute crossterm commands")?;

    let previous_hook = panic::take_hook();

    panic::set_hook(Box::new(move |panic_info| {
        exit()
            .context("Failed to exit terminal during panic")
            .unwrap();
        previous_hook(panic_info);
    }));

    let backend = CrosstermBackend::new(stdout);

    let terminal = Terminal::new(backend).context("Failed to create ratatui terminal")?;

    Ok(TerminalGuard { terminal })
}

pub fn exit() -> anyhow::Result<()> {
    let mut stdout = std::io::stdout();

    crossterm::execute!(
        stdout,
        event::PopKeyboardEnhancementFlags,
        event::DisableBracketedPaste,
        event::DisableMouseCapture,
        cursor::Show,
        terminal::LeaveAlternateScreen,
    )
    .context("Failed to execute crossterm commands")?;

    terminal::disable_raw_mode().context("Failed to disable raw mode")?;

    if !std::thread::panicking() {
        let _ = panic::take_hook();
    }

    Ok(())
}
