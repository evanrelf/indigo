mod buffer;
mod conversion;
mod crdt;
mod editor;
mod history;
mod mode;
mod position;
mod rope;
mod selection;
mod terminal;

use crate::{editor::Editor, mode::Mode, position::Position, rope::RopeExt as _};
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use ratatui::{
    prelude::{Buffer as Surface, Color, Constraint, Layout, Rect, Style, Widget as _},
    widgets::Paragraph,
};
use ropey::Rope;
use std::{
    borrow::Cow,
    cmp::{max, min},
    fs::File,
    io::BufReader,
};

#[derive(clap::Parser, Debug)]
struct Args {
    file: Option<Utf8PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut editor = Editor::default();

    if let Some(path) = args.file {
        let file = File::open(&path)?;
        editor.path = Some(path);
        editor.buffer.text = Rope::from_reader(BufReader::new(file))?;
    }

    let mut terminal = terminal::enter()?;

    loop {
        let mut result = Ok(());

        terminal.draw(|frame| {
            let area = frame.size();
            let surface = frame.buffer_mut();
            result = render(&editor, area, surface);
        })?;

        result?;

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
                (KeyModifiers::NONE, KeyCode::Char('H')) => buffer.extend_left(1)?,
                (KeyModifiers::NONE, KeyCode::Char('J')) => buffer.extend_down(1)?,
                (KeyModifiers::NONE, KeyCode::Char('K')) => buffer.extend_up(1)?,
                (KeyModifiers::NONE, KeyCode::Char('L')) => buffer.extend_right(1)?,
                (KeyModifiers::SHIFT, KeyCode::Char('H')) => buffer.extend_left(1)?,
                (KeyModifiers::SHIFT, KeyCode::Char('J')) => buffer.extend_down(1)?,
                (KeyModifiers::SHIFT, KeyCode::Char('K')) => buffer.extend_up(1)?,
                (KeyModifiers::SHIFT, KeyCode::Char('L')) => buffer.extend_right(1)?,
                (KeyModifiers::NONE, KeyCode::Char(';')) => buffer.selection.reduce(),
                (KeyModifiers::ALT, KeyCode::Char(';')) => buffer.selection.flip(),
                (KeyModifiers::NONE, KeyCode::Char('i')) => editor.mode = Mode::Insert,
                (KeyModifiers::NONE, KeyCode::Char('a')) => {
                    buffer.move_right(1)?;
                    editor.mode = Mode::Insert;
                }
                (KeyModifiers::NONE, KeyCode::Up) => editor.scroll_up(1),
                (KeyModifiers::NONE, KeyCode::Down) => editor.scroll_down(1),
                (KeyModifiers::NONE, KeyCode::Left) => editor.scroll_left(1),
                (KeyModifiers::NONE, KeyCode::Right) => editor.scroll_right(1),
                (KeyModifiers::CONTROL, KeyCode::Char('s')) => editor.save()?,
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
                (KeyModifiers::CONTROL, KeyCode::Char('s')) => editor.save()?,
                (KeyModifiers::NONE, KeyCode::Esc) => editor.mode = Mode::Normal,
                _ => {}
            },
            Event::Paste(string) => buffer.insert(string)?,
            _ => {}
        },
    }

    Ok(quit)
}

fn render(editor: &Editor, area: Rect, surface: &mut Surface) -> anyhow::Result<()> {
    let areas = Layout::vertical([Constraint::Fill(1), Constraint::Length(1)]).split(area);

    let buffer_area = areas[0];
    let status_bar_area = areas[1];

    render_text(editor, buffer_area, surface)?;
    render_selection(editor, buffer_area, surface)?;
    render_status_bar(editor, status_bar_area, surface);

    Ok(())
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

fn render_selection(editor: &Editor, area: Rect, surface: &mut Surface) -> anyhow::Result<()> {
    let buffer = &editor.buffer;

    let anchor_index = buffer.selection.anchor.to_char_index(&editor.buffer.text)?;
    let cursor_index = buffer.selection.cursor.to_char_index(&editor.buffer.text)?;
    let start_index = min(anchor_index, cursor_index);
    let end_index = max(anchor_index, cursor_index);

    for index in start_index..=end_index {
        let position = Position::from_char_index(index, &editor.buffer.text)?;

        if editor.scroll.line > position.line {
            return Ok(());
        }

        let line = u16::try_from(position.line - editor.scroll.line)? + area.top();

        if !(area.top()..area.bottom()).contains(&line) {
            return Ok(());
        }

        if editor.scroll.column > position.column {
            return Ok(());
        }

        let column = u16::try_from(position.column - editor.scroll.column)? + area.left();

        if !(area.left()..area.right()).contains(&column) {
            return Ok(());
        }

        if index == cursor_index {
            surface
                .get_mut(column, line)
                .set_fg(Color::Black)
                .set_bg(Color::Rgb(0xFF, 0xD3, 0x3D));
        } else {
            surface
                .get_mut(column, line)
                .set_fg(Color::Black)
                .set_bg(Color::Rgb(0xFF, 0xF5, 0xB1));
        }
    }

    Ok(())
}

fn render_status_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    let mode = match editor.mode {
        Mode::Normal => "N",
        Mode::Insert => "I",
    };

    let path = match &editor.path {
        Some(path) => path.as_str(),
        None => "",
    };

    let x = area.left();
    let y = area.bottom() - 1;
    let string = format!("{mode} {path}");
    let style = Style::new().fg(Color::Blue);
    surface.set_string(x, y, string, style);
}
