mod editor;
mod terminal;

use crate::{editor::Editor, terminal::Terminal};
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use ratatui::widgets::Paragraph;
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

    let mut terminal = Terminal::new()?;

    terminal::enter()?;

    loop {
        terminal.draw(|frame| {
            frame.render_widget(
                Paragraph::new("Hello, world! (Ctrl-C to exit)"),
                frame.size(),
            );
        })?;

        #[allow(clippy::single_match)]
        match event::read()? {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (KeyModifiers::CONTROL, KeyCode::Char('c')) => break,
                _ => {}
            },
            _ => {}
        }
    }

    terminal::exit()?;

    Ok(())
}
