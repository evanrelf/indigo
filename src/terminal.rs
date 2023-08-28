use crossterm::{cursor, event, terminal, QueueableCommand as _};
use std::{
    io::{self, Write as _},
    panic,
};

pub struct TerminalGuard;

impl Drop for TerminalGuard {
    fn drop(&mut self) {
        exit();
    }
}

pub fn enter() -> TerminalGuard {
    use event::KeyboardEnhancementFlags as KEF;

    let mut stdout = io::stdout();

    terminal::enable_raw_mode().unwrap();
    stdout.queue(terminal::EnterAlternateScreen).unwrap();
    stdout.queue(cursor::Hide).unwrap();
    stdout.queue(event::EnableMouseCapture).unwrap();
    stdout
        .queue(event::PushKeyboardEnhancementFlags(
            KEF::DISAMBIGUATE_ESCAPE_CODES | KEF::REPORT_EVENT_TYPES,
        ))
        .unwrap();
    stdout.flush().unwrap();

    let previous_hook = panic::take_hook();
    panic::set_hook(Box::new(move |panic_info| {
        exit();
        previous_hook(panic_info);
    }));

    TerminalGuard
}

pub fn exit() {
    let mut stdout = io::stdout();

    stdout.queue(event::PopKeyboardEnhancementFlags).unwrap();
    stdout.queue(event::DisableMouseCapture).unwrap();
    stdout.queue(cursor::Show).unwrap();
    stdout.queue(terminal::LeaveAlternateScreen).unwrap();
    stdout.flush().unwrap();
    terminal::disable_raw_mode().unwrap();

    if !std::thread::panicking() {
        let _ = panic::take_hook();
    }
}
