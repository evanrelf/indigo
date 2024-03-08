mod editor;

use crate::editor::Editor;
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use ropey::Rope;
use std::{fs::File, io::BufReader};

#[derive(clap::Parser, Debug)]
struct Args {
    file: Option<Utf8PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut editor = Editor::default();

    if let Some(ref path) = args.file {
        let file = File::open(path)?;
        editor.text = Rope::from_reader(BufReader::new(file))?;
    }

    enter_terminal()?;

    crossterm::execute!(
        std::io::stdout(),
        crossterm::cursor::MoveTo(0, 0),
        crossterm::style::Print("Hello, world! (Ctrl-C to exit)"),
    )?;

    loop {
        #[allow(clippy::single_match)]
        match event::read()? {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (KeyModifiers::CONTROL, KeyCode::Char('c')) => break,
                _ => {}
            },
            _ => {}
        }
    }

    exit_terminal()?;

    Ok(())
}

fn enter_terminal() -> anyhow::Result<()> {
    let mut stdout = std::io::stdout();

    crossterm::terminal::enable_raw_mode()?;

    crossterm::execute!(
        stdout,
        crossterm::terminal::EnterAlternateScreen,
        crossterm::cursor::Hide,
    )?;

    Ok(())
}

fn exit_terminal() -> anyhow::Result<()> {
    let mut stdout = std::io::stdout();

    crossterm::execute!(
        stdout,
        crossterm::cursor::Show,
        crossterm::terminal::LeaveAlternateScreen,
    )?;

    crossterm::terminal::disable_raw_mode()?;

    Ok(())
}
