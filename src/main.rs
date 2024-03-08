mod editor;
mod terminal;

use crate::{editor::Editor, terminal::Terminal};
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use ratatui::widgets::Paragraph;
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
        terminal.draw(|frame| {
            let text = Cow::<'_, str>::from(&editor.text);
            let last_line = len_lines_indigo(&editor.text).saturating_sub(1);
            let scroll = min(last_line, editor.scroll);
            let scroll = u16::try_from(scroll).unwrap();
            frame.render_widget(Paragraph::new(text).scroll((scroll, 0)), frame.size());
        })?;

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

fn len_lines_indigo(rope: &Rope) -> usize {
    if rope.len_chars() == 0 {
        return 0;
    }
    let last_char = rope.char(rope.len_chars() - 1);
    rope.len_lines() - if last_char == '\n' { 1 } else { 0 }
}
