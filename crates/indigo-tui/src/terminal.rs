use anyhow::Context as _;
use crossterm::{cursor, event, terminal};
use ratatui::prelude::{CrosstermBackend, Terminal};
use std::{
    io::Stdout,
    ops::{Deref, DerefMut},
    panic,
    sync::atomic::{AtomicBool, Ordering},
};

pub struct TerminalGuard {
    terminal: Terminal<CrosstermBackend<Stdout>>,
}

impl TerminalGuard {
    fn new() -> anyhow::Result<Self> {
        let stdout = std::io::stdout();

        let backend = CrosstermBackend::new(stdout);

        let terminal = Terminal::new(backend).context("Failed to create ratatui terminal")?;

        Ok(Self { terminal })
    }
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
        let result = exit().context("Failed to exit terminal during drop");
        // Ignore the `Result` if we're already panicking; aborting is undesirable
        if !std::thread::panicking() {
            result.unwrap();
        }
    }
}

static INSIDE: AtomicBool = AtomicBool::new(false);

pub fn enter() -> anyhow::Result<TerminalGuard> {
    use event::KeyboardEnhancementFlags as KEF;

    if INSIDE.load(Ordering::SeqCst) {
        return TerminalGuard::new();
    }

    INSIDE.store(true, Ordering::SeqCst);

    let mut stdout = std::io::stdout();

    terminal::enable_raw_mode().context("Failed to enable raw mode")?;

    crossterm::execute!(
        stdout,
        terminal::EnterAlternateScreen,
        cursor::Hide,
        event::EnableMouseCapture,
        event::EnableFocusChange,
        event::EnableBracketedPaste,
        event::PushKeyboardEnhancementFlags(KEF::DISAMBIGUATE_ESCAPE_CODES),
    )
    .context("Failed to execute crossterm commands")?;

    let previous_hook = panic::take_hook();

    panic::set_hook(Box::new(move |panic_info| {
        // Ignoring the `Result` because we're already panicking; aborting is undesirable
        let _ = exit();
        previous_hook(panic_info);
    }));

    TerminalGuard::new()
}

pub fn exit() -> anyhow::Result<()> {
    if !INSIDE.load(Ordering::SeqCst) {
        return Ok(());
    }

    let mut stdout = std::io::stdout();

    crossterm::execute!(
        stdout,
        event::PopKeyboardEnhancementFlags,
        event::DisableBracketedPaste,
        event::DisableFocusChange,
        event::DisableMouseCapture,
        cursor::Show,
        terminal::LeaveAlternateScreen,
    )
    .context("Failed to execute crossterm commands")?;

    terminal::disable_raw_mode().context("Failed to disable raw mode")?;

    INSIDE.store(false, Ordering::SeqCst);

    Ok(())
}
