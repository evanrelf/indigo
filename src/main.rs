mod terminal;

use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use ratatui::{
    prelude::{Buffer as Surface, Rect, Widget as _},
    widgets::Paragraph,
};
use ropey::Rope;
use std::{fs::File, io::BufReader};

#[derive(Debug, clap::Parser)]
struct Args {
    file: Utf8PathBuf,
}

struct Editor {
    text: Rope,
    // Byte offset
    cursor: usize,
}

impl Editor {
    fn new(path: Utf8PathBuf) -> anyhow::Result<Self> {
        let file = File::open(path)?;
        let rope = Rope::from_reader(BufReader::new(file))?;
        Ok(Self {
            text: rope,
            cursor: 0,
        })
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut editor = Editor::new(args.file)?;

    let mut terminal = terminal::enter()?;

    let mut area = Rect::default();

    loop {
        terminal.draw(|frame| {
            area = frame.size();
            let surface = frame.buffer_mut();
            render(&editor, area, surface);
        })?;

        let quit = handle_event(&mut editor, area, &event::read()?);

        if quit {
            break;
        }
    }

    Ok(())
}

fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    Paragraph::new(format!("Cursor: {}\n{}", editor.cursor, editor.text)).render(area, surface);
}

fn handle_event(editor: &mut Editor, _area: Rect, event: &Event) -> bool {
    let mut quit = false;

    #[allow(clippy::single_match)]
    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Char('h')) => {
                editor.cursor = editor.cursor.saturating_sub(1);
            }
            (KeyModifiers::NONE, KeyCode::Char('l')) => {
                editor.cursor += 1;
            }
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => quit = true,
            _ => {}
        },
        _ => {}
    }

    quit
}
