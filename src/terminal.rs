use crossterm::{Command, ExecutableCommand, QueueableCommand};
use std::{
    io::{stdout, Stdout, Write},
    panic,
};

pub struct Terminal {}

impl Terminal {
    pub fn execute<C>(command: C)
    where
        C: Command,
    {
        stdout().execute(command).unwrap();
    }

    pub fn queue<C>(command: C)
    where
        C: Command,
    {
        stdout().queue(command).unwrap();
    }

    pub fn flush() {
        stdout().flush().unwrap();
    }
}

pub fn with_terminal<F>(f: F)
where
    F: FnOnce(&mut tui::Terminal<tui::backend::CrosstermBackend<Stdout>>),
{
    let mut terminal = {
        let stdout = std::io::stdout();
        let backend = tui::backend::CrosstermBackend::new(stdout);
        tui::Terminal::new(backend).unwrap()
    };

    enter_terminal();

    f(&mut terminal);

    exit_terminal(false);
}

fn enter_terminal() {
    let mut stdout = std::io::stdout();
    crossterm::terminal::enable_raw_mode().unwrap();
    stdout
        .execute(crossterm::terminal::EnterAlternateScreen)
        .unwrap();
    stdout.execute(crossterm::cursor::Hide).unwrap();
    panic::set_hook(Box::new(|panic_info| {
        exit_terminal(true);
        eprintln!("{}", panic_info);
    }));
}

fn exit_terminal(panicking: bool) {
    let mut stdout = std::io::stdout();
    stdout.queue(crossterm::cursor::Show).unwrap();
    stdout
        .queue(crossterm::terminal::LeaveAlternateScreen)
        .unwrap();
    stdout.flush().unwrap();
    crossterm::terminal::disable_raw_mode().unwrap();
    if !panicking {
        let _ = panic::take_hook();
    }
}
