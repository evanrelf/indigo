mod conversion;
mod editor;
mod mode;
mod position;
mod rope;
mod terminal;

use crate::{editor::Editor, mode::Mode, rope::RopeExt as _, terminal::Terminal};
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

        let quit = handle_event(&mut editor, &event::read()?)?;

        if quit {
            break;
        }
    }

    terminal::exit()?;

    Ok(())
}

fn handle_event(editor: &mut Editor, event: &Event) -> anyhow::Result<bool> {
    let mut quit = false;

    #[allow(clippy::single_match)]
    match &editor.mode {
        Mode::Normal => match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (KeyModifiers::NONE, KeyCode::Char('h')) => editor.move_left(1)?,
                (KeyModifiers::NONE, KeyCode::Char('j')) => editor.move_down(1)?,
                (KeyModifiers::NONE, KeyCode::Char('k')) => editor.move_up(1)?,
                (KeyModifiers::NONE, KeyCode::Char('l')) => editor.move_right(1)?,
                (KeyModifiers::NONE, KeyCode::Char('i')) => editor.mode = Mode::Insert,
                (KeyModifiers::NONE, KeyCode::Char('a')) => {
                    editor.move_right(1)?;
                    editor.mode = Mode::Insert;
                }
                (KeyModifiers::NONE, KeyCode::Up) => editor.scroll_up(1),
                (KeyModifiers::NONE, KeyCode::Down) => editor.scroll_down(1),
                (KeyModifiers::NONE, KeyCode::Left) => editor.scroll_left(1),
                (KeyModifiers::NONE, KeyCode::Right) => editor.scroll_right(1),
                (KeyModifiers::CONTROL, KeyCode::Char('c')) => quit = true,
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
                (KeyModifiers::NONE, KeyCode::Char(c)) => editor.insert_char(c)?,
                (KeyModifiers::SHIFT, KeyCode::Char(c)) => editor.insert_char(c)?,
                (KeyModifiers::NONE, KeyCode::Enter) => editor.insert_char('\n')?,
                (KeyModifiers::NONE, KeyCode::Backspace) => editor.backspace()?,
                (KeyModifiers::NONE, KeyCode::Esc) => editor.mode = Mode::Normal,
                _ => {}
            },
            _ => {}
        },
    }

    Ok(quit)
}

fn render_text(editor: &Editor, area: Rect, buffer: &mut Buffer) -> anyhow::Result<()> {
    let text = Cow::<'_, str>::from(&editor.text);

    let last_line = editor.text.len_lines_indigo().saturating_sub(1);

    let scroll_line = u16::try_from(min(last_line, editor.scroll.line))?;
    let scroll_column = u16::try_from(editor.scroll.column)?;

    Paragraph::new(text)
        .scroll((scroll_line, scroll_column))
        .render(area, buffer);

    Ok(())
}

fn render_cursor(editor: &Editor, area: Rect, buffer: &mut Buffer) -> anyhow::Result<()> {
    if editor.scroll.line > editor.cursor.line {
        return Ok(());
    }

    let line = u16::try_from(editor.cursor.line - editor.scroll.line)? + area.top();

    if !(area.top()..area.bottom()).contains(&line) {
        return Ok(());
    }

    if editor.scroll.column > editor.cursor.column {
        return Ok(());
    }

    let column = u16::try_from(editor.cursor.column - editor.scroll.column)? + area.left();

    if !(area.left()..area.right()).contains(&column) {
        return Ok(());
    }

    buffer
        .get_mut(column, line)
        .set_fg(Color::White)
        .set_bg(Color::Black);

    Ok(())
}
