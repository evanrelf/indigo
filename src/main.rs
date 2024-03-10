mod editor;
mod rope;
mod terminal;

use crate::{editor::Editor, rope::RopeExt as _, terminal::Terminal};
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use ratatui::{
    prelude::{Buffer, Color, Rect, Widget as _},
    widgets::Paragraph,
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
        terminal.draw(|frame| {
            let area = frame.size();
            let buffer = frame.buffer_mut();
            render_text(&editor, area, buffer).unwrap();
            render_cursor(&editor, area, buffer).unwrap();
        })?;

        #[allow(clippy::single_match)]
        match event::read()? {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (KeyModifiers::NONE, KeyCode::Char('h')) => editor.move_left(1),
                (KeyModifiers::NONE, KeyCode::Char('j')) => editor.move_down(1),
                (KeyModifiers::NONE, KeyCode::Char('k')) => editor.move_up(1),
                (KeyModifiers::NONE, KeyCode::Char('l')) => editor.move_right(1),
                (KeyModifiers::CONTROL, KeyCode::Char('c')) => break,
                _ => {}
            },
            Event::Mouse(mouse_event) => match mouse_event.kind {
                MouseEventKind::ScrollUp => editor.scroll_up(3),
                MouseEventKind::ScrollDown => editor.scroll_down(3),
                _ => {}
            },
            _ => {}
        }
    }

    terminal::exit()?;

    Ok(())
}

fn render_text(editor: &Editor, area: Rect, buffer: &mut Buffer) -> anyhow::Result<()> {
    let text = Cow::<'_, str>::from(&editor.text);

    let last_line = editor.text.len_lines_indigo().saturating_sub(1);

    let (scroll_line, scroll_column) = editor.scroll;

    let scroll_line = u16::try_from(min(last_line, scroll_line))?;
    let scroll_column = u16::try_from(scroll_column)?;

    Paragraph::new(text)
        .scroll((scroll_line, scroll_column))
        .render(area, buffer);

    Ok(())
}

fn render_cursor(editor: &Editor, area: Rect, buffer: &mut Buffer) -> anyhow::Result<()> {
    let (line, column) = editor.cursor;
    let (scroll_line, scroll_column) = editor.scroll;

    if scroll_line > line {
        return Ok(());
    }

    let line = u16::try_from(line - scroll_line)? + area.top();

    if !(area.top()..area.bottom()).contains(&line) {
        return Ok(());
    }

    if scroll_column > column {
        return Ok(());
    }

    let column = u16::try_from(column - scroll_column)? + area.left();

    if !(area.left()..area.right()).contains(&column) {
        return Ok(());
    }

    buffer
        .get_mut(column, line)
        .set_fg(Color::White)
        .set_bg(Color::Black);

    Ok(())
}
