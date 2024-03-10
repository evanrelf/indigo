use ratatui::{backend::CrosstermBackend, Terminal};
use std::{
    io::Stdout,
    ops::{Deref, DerefMut},
    sync::atomic::{AtomicBool, Ordering},
};

// The steps performed by `enter` and `exit` appear to be idempotent, however using multiple
// `ratatui::Terminal`s is ill advised.

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
        // If we're panicking, the panic hook installed by `enter` will have already called `exit`.
        if !std::thread::panicking() {
            // It's okay to `unwrap` because we're not panicking, so this won't turn into an abort.
            exit().unwrap();
        }
    }
}

pub fn enter() -> anyhow::Result<TerminalGuard> {
    // Ensure the panic hook isn't installed more than once, because we have no safe or guaranteed
    // way of uninstalling it.
    static PANIC_HOOK_INSTALLED: AtomicBool = AtomicBool::new(false);

    let mut stdout = std::io::stdout();

    if !PANIC_HOOK_INSTALLED.load(Ordering::SeqCst) {
        let previous_hook = std::panic::take_hook();

        // Restoring terminal state before the panic hook runs so that its output shows up.
        std::panic::set_hook(Box::new(move |panic_info| {
            // Ignoring the `Result` because we're already panicking; aborting is undesirable.
            let _ = exit();
            previous_hook(panic_info);
        }));

        PANIC_HOOK_INSTALLED.store(true, Ordering::SeqCst);
    }

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
