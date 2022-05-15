use crossterm::ExecutableCommand as _;
use std::{
    io::{stdout, Stdout, Write},
    ops::{Deref, DerefMut},
    panic,
};

pub struct Terminal {
    tui_terminal: tui::Terminal<tui::backend::CrosstermBackend<Stdout>>,
}

impl Terminal {
    pub fn new() -> Self {
        let tui_terminal = {
            let stdout = stdout();
            let backend = tui::backend::CrosstermBackend::new(stdout);
            tui::Terminal::new(backend).unwrap()
        };

        Self::enter();

        Self { tui_terminal }
    }

    fn enter() {
        use crossterm::{cursor, event, terminal};

        let mut stdout = stdout();
        terminal::enable_raw_mode().unwrap();
        stdout.execute(terminal::EnterAlternateScreen).unwrap();
        stdout.execute(cursor::Hide).unwrap();
        stdout.execute(event::EnableMouseCapture).unwrap();

        let default_hook = panic::take_hook();
        panic::set_hook(Box::new(move |panic_info| {
            Self::exit();
            default_hook(panic_info);
        }));
    }

    fn exit() {
        use crossterm::{cursor, event, terminal};

        let mut stdout = stdout();
        stdout.execute(event::DisableMouseCapture).unwrap();
        stdout.execute(cursor::Show).unwrap();
        stdout.execute(terminal::LeaveAlternateScreen).unwrap();
        stdout.flush().unwrap();
        terminal::disable_raw_mode().unwrap();

        if !std::thread::panicking() {
            let _ = panic::take_hook();
        }
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        Self::exit();
    }
}

impl Deref for Terminal {
    type Target = tui::Terminal<tui::backend::CrosstermBackend<Stdout>>;

    fn deref(&self) -> &Self::Target {
        &self.tui_terminal
    }
}

impl DerefMut for Terminal {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tui_terminal
    }
}
