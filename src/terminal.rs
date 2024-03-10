use ratatui::{backend::CrosstermBackend, Terminal};
use std::{
    io::Stdout,
    ops::{Deref, DerefMut},
};

pub struct TerminalGuard(Terminal<CrosstermBackend<Stdout>>);

impl TerminalGuard {
    fn new(stdout: Stdout) -> anyhow::Result<Self> {
        let backend = CrosstermBackend::new(stdout);
        let terminal = Terminal::new(backend)?;
        Ok(Self(terminal))
    }
}

impl Deref for TerminalGuard {
    type Target = Terminal<CrosstermBackend<Stdout>>;

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
        let result = exit();
        // Ignore the `Result` if we're already panicking; aborting is undesirable.
        if !std::thread::panicking() {
            result.unwrap();
        }
    }
}

pub fn enter() -> anyhow::Result<TerminalGuard> {
    let mut stdout = std::io::stdout();

    crossterm::terminal::enable_raw_mode()?;

    crossterm::execute!(
        stdout,
        crossterm::terminal::EnterAlternateScreen,
        crossterm::cursor::Hide,
        crossterm::event::EnableMouseCapture,
    )?;

    let terminal_guard = TerminalGuard::new(stdout)?;

    Ok(terminal_guard)
}

pub fn exit() -> anyhow::Result<()> {
    let mut stdout = std::io::stdout();

    crossterm::execute!(
        stdout,
        crossterm::event::DisableMouseCapture,
        crossterm::cursor::Show,
        crossterm::terminal::LeaveAlternateScreen,
    )?;

    crossterm::terminal::disable_raw_mode()?;

    Ok(())
}
