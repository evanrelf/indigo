mod editor;
mod rope;
mod terminal;

use crate::{editor::Editor, rope::RopeExt as _, terminal::Terminal};
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use ratatui::{
    prelude::{Buffer, Rect},
    widgets::{Paragraph, Widget as _},
};
use ropey::Rope;
use std::{borrow::Cow, cmp::min, fs::File, io::BufReader};

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
        terminal.draw(|frame| render(&editor, frame.size(), frame.buffer_mut()).unwrap())?;

        #[allow(clippy::single_match)]
        match event::read()? {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (KeyModifiers::CONTROL, KeyCode::Char('c')) => break,
                _ => {}
            },
            Event::Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => editor.scroll = editor.scroll.saturating_sub(3),
                MouseEventKind::ScrollDown => editor.scroll += 3,
                _ => {}
            },
            _ => {}
        }
    }

    terminal::exit()?;

    Ok(())
}

fn render(editor: &Editor, area: Rect, buffer: &mut Buffer) -> anyhow::Result<()> {
    let text = Cow::<'_, str>::from(&editor.text);

    let last_line = editor.text.len_lines_indigo().saturating_sub(1);

    let scroll = u16::try_from(min(last_line, editor.scroll))?;

    Paragraph::new(text)
        .scroll((scroll, 0))
        .render(area, buffer);

    Ok(())
}
