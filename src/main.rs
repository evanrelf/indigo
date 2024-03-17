mod terminal;

use anyhow::Context as _;
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use indigo::{Buffer, Editor, Mode, Position, RopeExt as _};
use ratatui::prelude::{Buffer as Surface, Color, Constraint, Layout, Rect, Style};
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
        editor.buffer = Buffer::new(Rope::from_reader(BufReader::new(file))?)?;
    }

    let mut terminal = terminal::enter()?;

    loop {
        let mut areas = Areas::default();
        let mut result = Ok(());

        terminal.draw(|frame| {
            areas = Areas::new(frame.size());
            let surface = frame.buffer_mut();
            result = render(&editor, areas, surface);
        })?;

        result?;

        let quit = handle_event(&mut editor, areas, &event::read()?)?;

        if quit {
            break;
        }
    }

    Ok(())
}

#[derive(Clone, Copy, Default)]
struct Areas {
    buffer: Rect,
    status_bar: Rect,
}

impl Areas {
    fn new(area: Rect) -> Self {
        let areas = Layout::vertical([Constraint::Fill(1), Constraint::Length(1)]).split(area);

        Self {
            buffer: areas[0],
            status_bar: areas[1],
        }
    }
}

// TODO: Pick a style
enum InsertStyle {
    /// Flip the selection to face the direction of insertion. You can think of the entire selection
    /// like a "fat cursor". This is how Kakoune works.
    Flip,

    /// Keep the selection as is, allowing text to be inserted inside the initial selection range.
    /// The cursor remains in place like `Reduce`, but the rest of the selection is kept.
    Stay,

    /// Reduce the selection when entering insert mode. The cursor remains in place like `Stay`, but
    /// the rest of the selection is lost.
    Reduce,
}

fn show_cursor(editor: &mut Editor, areas: Areas) {
    let area = areas.buffer;

    if area.height == 0 {
        return;
    }

    let cursor_line = editor.buffer.selection().cursor.line;
    let last_line = editor.buffer.text().len_lines_indigo() - 1;
    let top_line = editor.scroll.line;
    let bottom_line = editor.scroll.line + usize::from(area.height - 1);

    if cursor_line < top_line {
        editor.scroll.line = min(cursor_line, last_line);
    } else if cursor_line > bottom_line {
        editor.scroll.line = min(cursor_line - usize::from(area.height - 1), last_line);
    }
}

fn handle_event(editor: &mut Editor, areas: Areas, event: &Event) -> anyhow::Result<bool> {
    let area = areas.buffer;

    let insert_style = InsertStyle::Reduce;

    let mut quit = false;

    let buffer = &mut editor.buffer;

    #[allow(clippy::single_match, clippy::match_same_arms)]
    match &editor.mode {
        Mode::Normal => match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (KeyModifiers::NONE, KeyCode::Char(' ')) => show_cursor(editor, areas),
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
                (KeyModifiers::NONE, KeyCode::Char('u')) => buffer.undo(),
                (KeyModifiers::NONE, KeyCode::Char('U')) => buffer.redo(),
                (KeyModifiers::SHIFT, KeyCode::Char('U')) => buffer.redo(),
                (KeyModifiers::NONE, KeyCode::Char('i')) => {
                    match insert_style {
                        InsertStyle::Flip => buffer.selection.flip_backward(),
                        InsertStyle::Stay => {}
                        InsertStyle::Reduce => buffer.selection.reduce(),
                    }
                    buffer.record();
                    editor.mode = Mode::Insert { after: false };
                }
                (KeyModifiers::NONE, KeyCode::Char('a')) => {
                    match insert_style {
                        InsertStyle::Flip => {
                            buffer.selection.flip_forward();
                            buffer.extend_right(1)?;
                        }
                        InsertStyle::Stay => {
                            buffer.extend_right(1)?;
                        }
                        InsertStyle::Reduce => {
                            buffer.selection.reduce();
                            buffer.move_right(1)?;
                        }
                    }
                    buffer.record();
                    editor.mode = Mode::Insert { after: true };
                }
                (KeyModifiers::NONE, KeyCode::Up) => editor.scroll_up(1),
                (KeyModifiers::NONE, KeyCode::Down) => editor.scroll_down(1),
                (KeyModifiers::NONE, KeyCode::Left) => editor.scroll_left(1),
                (KeyModifiers::NONE, KeyCode::Right) => editor.scroll_right(1),
                (KeyModifiers::CONTROL, KeyCode::Char('d')) => {
                    editor.scroll_down(usize::from(area.height / 2));
                }
                (KeyModifiers::CONTROL, KeyCode::Char('u')) => {
                    editor.scroll_up(usize::from(area.height / 2));
                }
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
        Mode::Insert { after } => match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (KeyModifiers::NONE, KeyCode::Char(char)) => buffer.insert_char(char)?,
                (KeyModifiers::SHIFT, KeyCode::Char(char)) => buffer.insert_char(char)?,
                (KeyModifiers::NONE, KeyCode::Enter) => buffer.insert_char('\n')?,
                (KeyModifiers::NONE, KeyCode::Backspace) => buffer.backspace()?,
                (KeyModifiers::CONTROL, KeyCode::Char('s')) => editor.save()?,
                (KeyModifiers::NONE, KeyCode::Esc) => {
                    if *after && !buffer.selection().is_reduced() {
                        buffer.extend_left(1)?;
                    }
                    buffer.record();
                    editor.mode = Mode::Normal;
                }
                _ => {}
            },
            Event::Paste(string) => buffer.insert(string)?,
            _ => {}
        },
    }

    Ok(quit)
}

fn render(editor: &Editor, areas: Areas, surface: &mut Surface) -> anyhow::Result<()> {
    render_text(editor, areas.buffer, surface);
    render_selection(editor, areas.buffer, surface)?;
    render_status_bar(editor, areas.status_bar, surface);

    Ok(())
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    if area.height == 0 || area.width == 0 {
        return;
    }

    for y in area.top()..area.bottom() {
        let line_index = usize::from(y) + editor.scroll.line;

        let Some(line) = editor.buffer.text().get_line(line_index) else {
            break;
        };

        // TODO: Use `chunks_at_char` instead of allocating a contiguous `&str`.
        let Some(line) = line.get_slice(editor.scroll.column..).map(Cow::<str>::from) else {
            continue;
        };

        surface.set_stringn(area.x, y, line, usize::from(area.width), Style::default());
    }
}

fn render_selection(editor: &Editor, area: Rect, surface: &mut Surface) -> anyhow::Result<()> {
    let buffer = &editor.buffer;

    let anchor_index = buffer
        .selection()
        .anchor
        .to_char_index(editor.buffer.text())?;
    let cursor_index = buffer
        .selection()
        .cursor
        .to_char_index(editor.buffer.text())?;
    let start_index = min(anchor_index, cursor_index);
    let end_index = max(anchor_index, cursor_index);

    for index in start_index..=end_index {
        let char = buffer.text().get_char(index).context("bad index")?;

        let position = Position::from_char_index(index, editor.buffer.text())?;

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

        let orange = Color::Rgb(0xD1, 0x58, 0x02);
        let dark_yellow = Color::Rgb(0xFF, 0xD3, 0x3D);
        let light_yellow = Color::Rgb(0xFF, 0xF5, 0xB1);

        let bg_color = if index == cursor_index && char == '\n' {
            orange
        } else if index == cursor_index {
            dark_yellow
        } else {
            light_yellow
        };

        surface
            .get_mut(column, line)
            .set_fg(Color::Black)
            .set_bg(bg_color);
    }

    Ok(())
}

fn render_status_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    let mode = match editor.mode {
        Mode::Normal => "N",
        Mode::Insert { .. } => "I",
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
