use crossterm::ExecutableCommand as _;
use std::{
    io::{stdout, Stdout, Write},
    panic,
};

pub fn with_terminal<F>(f: F)
where
    F: FnOnce(&mut tui::Terminal<tui::backend::CrosstermBackend<Stdout>>),
{
    let mut terminal = {
        let stdout = stdout();
        let backend = tui::backend::CrosstermBackend::new(stdout);
        tui::Terminal::new(backend).unwrap()
    };

    enter_terminal();

    f(&mut terminal);

    exit_terminal(false);
}

fn enter_terminal() {
    use crossterm::{cursor, event, terminal};

    let mut stdout = stdout();
    terminal::enable_raw_mode().unwrap();
    stdout.execute(terminal::EnterAlternateScreen).unwrap();
    stdout.execute(cursor::Hide).unwrap();
    stdout.execute(event::EnableMouseCapture).unwrap();

    let default_hook = Box::leak(panic::take_hook());
    panic::set_hook(Box::new(|panic_info| {
        exit_terminal(true);
        default_hook(panic_info);
    }));
}

fn exit_terminal(panicking: bool) {
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
