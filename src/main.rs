mod buffer;
mod conversion;
mod editor;
mod mode;
mod position;
mod rope;
mod terminal;

use crate::{editor::Editor, mode::Mode, rope::RopeExt as _};
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use ratatui::{
    prelude::{Buffer as Surface, Color, Rect, Widget as _},
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
        editor.buffer.text = Rope::from_reader(BufReader::new(file))?;
    }

    let mut terminal = terminal::enter()?;

    loop {
        terminal.draw(|frame| {
            let area = frame.size();
            let surface = frame.buffer_mut();
            render_text(&editor, area, surface).unwrap();
            render_cursor(&editor, area, surface).unwrap();
        })?;

        let quit = handle_event(&mut editor, &event::read()?)?;

        if quit {
            break;
        }
    }

    Ok(())
}

fn handle_event(editor: &mut Editor, event: &Event) -> anyhow::Result<bool> {
    let mut quit = false;

    let buffer = &mut editor.buffer;

    #[allow(clippy::single_match)]
    match &editor.mode {
        Mode::Normal => match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (KeyModifiers::NONE, KeyCode::Char('h')) => buffer.move_left(1)?,
                (KeyModifiers::NONE, KeyCode::Char('j')) => buffer.move_down(1)?,
                (KeyModifiers::NONE, KeyCode::Char('k')) => buffer.move_up(1)?,
                (KeyModifiers::NONE, KeyCode::Char('l')) => buffer.move_right(1)?,
                (KeyModifiers::NONE, KeyCode::Char('i')) => editor.mode = Mode::Insert,
                (KeyModifiers::NONE, KeyCode::Char('a')) => {
                    buffer.move_right(1)?;
                    editor.mode = Mode::Insert;
                }
                (KeyModifiers::NONE, KeyCode::Up) => editor.scroll_up(1),
                (KeyModifiers::NONE, KeyCode::Down) => editor.scroll_down(1),
                (KeyModifiers::NONE, KeyCode::Left) => editor.scroll_left(1),
                (KeyModifiers::NONE, KeyCode::Right) => editor.scroll_right(1),
                (KeyModifiers::CONTROL, KeyCode::Char('c')) => quit = true,
                (KeyModifiers::CONTROL, KeyCode::Char('p')) => panic!(),
                _ => {}
            },
            Event::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
                (KeyModifiers::ALT, MouseEventKind::ScrollUp) => editor.scroll_up(1),
                (KeyModifiers::ALT, MouseEventKind::ScrollDown) => editor.scroll_down(1),
                (KeyModifiers::NONE, MouseEventKind::ScrollUp) => editor.scroll_up(3),
                (KeyModifiers::NONE, MouseEventKind::ScrollDown) => editor.scroll_down(3),
                (KeyModifiers::SHIFT, MouseEventKind::ScrollUp) => editor.scroll_up(6),
                (KeyModifiers::SHIFT, MouseEventKind::ScrollDown) => editor.scroll_down(6),
                _ => {}
            },
            _ => {}
        },
        Mode::Insert => match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (KeyModifiers::NONE, KeyCode::Char(char)) => buffer.insert_char(char)?,
                (KeyModifiers::SHIFT, KeyCode::Char(char)) => buffer.insert_char(char)?,
                (KeyModifiers::NONE, KeyCode::Enter) => buffer.insert_char('\n')?,
                (KeyModifiers::NONE, KeyCode::Backspace) => buffer.backspace()?,
                (KeyModifiers::NONE, KeyCode::Esc) => editor.mode = Mode::Normal,
                _ => {}
            },
            Event::Paste(string) => buffer.insert(string)?,
            _ => {}
        },
    }

    Ok(quit)
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) -> anyhow::Result<()> {
    let buffer = &editor.buffer;

    let text = Cow::<'_, str>::from(&buffer.text);

    let last_line = buffer.text.len_lines_indigo().saturating_sub(1);

    let scroll_line = u16::try_from(min(last_line, editor.scroll.line))?;
    let scroll_column = u16::try_from(editor.scroll.column)?;

    Paragraph::new(text)
        .scroll((scroll_line, scroll_column))
        .render(area, surface);

    Ok(())
}

fn render_cursor(editor: &Editor, area: Rect, surface: &mut Surface) -> anyhow::Result<()> {
    let buffer = &editor.buffer;

    if editor.scroll.line > buffer.cursor.line {
        return Ok(());
    }

    let line = u16::try_from(buffer.cursor.line - editor.scroll.line)? + area.top();

    if !(area.top()..area.bottom()).contains(&line) {
        return Ok(());
    }

    if editor.scroll.column > buffer.cursor.column {
        return Ok(());
    }

    let column = u16::try_from(buffer.cursor.column - editor.scroll.column)? + area.left();

    if !(area.left()..area.right()).contains(&column) {
        return Ok(());
    }

    surface
        .get_mut(column, line)
        .set_fg(Color::White)
        .set_bg(Color::Black);

    Ok(())
}
