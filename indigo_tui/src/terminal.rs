use crossterm::ExecutableCommand as _;
use std::{
    io::{stdout, Stdout, Write},
    ops::{Deref, DerefMut},
    panic,
};

pub struct Terminal {
    tui_terminal: tui::Terminal<tui::backend::CrosstermBackend<Stdout>>,
    inside: bool,
}

impl Terminal {
    pub fn new() -> Self {
        let tui_terminal = {
            let stdout = stdout();
            let backend = tui::backend::CrosstermBackend::new(stdout);
            tui::Terminal::new(backend).unwrap()
        };

        Self {
            tui_terminal,
            inside: false,
        }
    }

    pub fn enter(&mut self) {
        use crossterm::{cursor, event, terminal};

        let mut stdout = stdout();
        terminal::enable_raw_mode().unwrap();
        stdout.execute(terminal::EnterAlternateScreen).unwrap();
        stdout.execute(cursor::Hide).unwrap();
        stdout.execute(event::EnableMouseCapture).unwrap();

        let default_hook = panic::take_hook();
        panic::set_hook(Box::new(move |panic_info| {
            Self::exit_impl(true);
            default_hook(panic_info);
        }));

        self.inside = true;
    }

    pub fn exit(&mut self) {
        Self::exit_impl(false);
        self.inside = false;
    }

    fn exit_impl(panicking: bool) {
        use crossterm::{cursor, event, terminal};

        let mut stdout = stdout();
        stdout.execute(event::DisableMouseCapture).unwrap();
        stdout.execute(cursor::Show).unwrap();
        stdout.execute(terminal::LeaveAlternateScreen).unwrap();
        stdout.flush().unwrap();
        terminal::disable_raw_mode().unwrap();

        if !panicking {
            let _ = panic::take_hook();
        }
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        if self.inside {
            Self::exit_impl(false);
        }
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
