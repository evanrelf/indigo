use anyhow::Context as _;
use crossterm::{cursor, event, terminal, QueueableCommand as _};
use ratatui::prelude::{CrosstermBackend, Terminal};
use std::{
    io::{Stdout, Write as _},
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

    stdout
        .queue(terminal::EnterAlternateScreen)
        .context("Failed to enter alternate screen")?
        .queue(cursor::Hide)
        .context("Failed to hide cursor")?
        .queue(event::EnableMouseCapture)
        .context("Failed to enable mouse capture")?
        .queue(event::PushKeyboardEnhancementFlags(
            KEF::DISAMBIGUATE_ESCAPE_CODES | KEF::REPORT_EVENT_TYPES,
        ))
        .context("Failed to push keyboard enhancement flags")?
        .flush()
        .context("Failed to flush queued commands")?;

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

    stdout
        .queue(event::PopKeyboardEnhancementFlags)
        .context("Failed to pop keyboard enhancement flags")?
        .queue(event::DisableMouseCapture)
        .context("Failed to disable mouse capture")?
        .queue(cursor::Show)
        .context("Failed to show cursor")?
        .queue(terminal::LeaveAlternateScreen)
        .context("Failed to leave alternate screen")?
        .flush()
        .context("Failed to flush queued commands")?;

    terminal::disable_raw_mode().context("Failed to disable raw mode")?;

    if !std::thread::panicking() {
        let _ = panic::take_hook();
    }

    Ok(())
}
