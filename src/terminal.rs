use crossterm::ExecutableCommand;
use std::{
    io::{Stdout, Write},
    panic,
};

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
    stdout
        .execute(crossterm::event::EnableMouseCapture)
        .unwrap();
    panic::set_hook(Box::new(|panic_info| {
        exit_terminal(true);
        eprintln!("{}", panic_info);
    }));
}

fn exit_terminal(panicking: bool) {
    let mut stdout = std::io::stdout();
    stdout
        .execute(crossterm::event::DisableMouseCapture)
        .unwrap();
    stdout.execute(crossterm::cursor::Show).unwrap();
    stdout
        .execute(crossterm::terminal::LeaveAlternateScreen)
        .unwrap();
    stdout.flush().unwrap();
    crossterm::terminal::disable_raw_mode().unwrap();
    if !panicking {
        let _ = panic::take_hook();
    }
}
