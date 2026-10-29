use ratatui::{
    Terminal,
    crossterm::{
        QueueableCommand as _,
        cursor::Show,
        event::{
            DisableBracketedPaste, DisableMouseCapture, EnableBracketedPaste, EnableMouseCapture,
            KeyboardEnhancementFlags, PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
        },
        terminal::{
            Clear, ClearType, EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode,
            enable_raw_mode, supports_keyboard_enhancement,
        },
    },
    prelude::CrosstermBackend,
};
use std::{
    io::{self, Write as _},
    ops::{Deref, DerefMut},
    panic,
    sync::atomic::{AtomicBool, Ordering},
    thread,
};

pub struct TerminalGuard(pub ratatui::DefaultTerminal);

static ENTERED: AtomicBool = AtomicBool::new(false);

pub fn init() -> anyhow::Result<TerminalGuard> {
    let main_thread = thread::current().id();
    let hook = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        if thread::current().id() == main_thread {
            leave();
        }
        hook(info);
    }));
    enter()?;
    let terminal = Terminal::new(CrosstermBackend::new(io::stdout()))?;
    Ok(TerminalGuard(terminal))
}

pub fn suspend(terminal: &mut TerminalGuard) -> anyhow::Result<()> {
    leave();
    // Stop our whole process group, like the terminal does for `^Z`, so wrappers like `cargo run`
    // stop too and the shell regains control.
    // SAFETY: `kill` has no preconditions beyond receiving a valid signal number.
    #[expect(unsafe_code)]
    unsafe {
        libc::kill(0, libc::SIGTSTP);
    }
    // Execution resumes here once the process is continued (e.g. with `fg`).
    enter()?;
    // `enter` cleared the screen, so Ratatui's previous frame is stale. Force a full redraw.
    terminal.clear()?;
    Ok(())
}

fn enter() -> anyhow::Result<()> {
    ENTERED.store(true, Ordering::Relaxed);
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
    Ok(())
}

fn leave() {
    if !ENTERED.swap(false, Ordering::Relaxed) {
        return;
    }
    let mut stdout = io::stdout();
    let _ = stdout.queue(PopKeyboardEnhancementFlags);
    let _ = stdout.queue(DisableBracketedPaste);
    let _ = stdout.queue(DisableMouseCapture);
    let _ = stdout.queue(Show);
    let _ = stdout.flush();
    let _ = disable_raw_mode();
    let _ = stdout.queue(LeaveAlternateScreen);
    let _ = stdout.flush();
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
        leave();
    }
}
