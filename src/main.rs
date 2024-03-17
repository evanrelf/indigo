mod terminal;

use anyhow::Context as _;
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, MouseEventKind};
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

#[allow(clippy::too_many_lines)]
fn handle_event(editor: &mut Editor, areas: Areas, event: &Event) -> anyhow::Result<bool> {
    #[allow(clippy::enum_glob_use)]
    use crossterm::event::{KeyCode::*, KeyModifiers as M};

    let area = areas.buffer;

    let insert_style = InsertStyle::Reduce;

    let mut quit = false;

    let buffer = &mut editor.buffer;

    #[allow(
        clippy::single_match,
        clippy::match_single_binding,
        clippy::match_same_arms
    )]
    match &editor.mode {
        Mode::Normal => match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (M::NONE, Char(' ')) => show_cursor(editor, areas),
                (M::NONE, Char('h')) => buffer.move_left(1)?,
                (M::NONE, Char('j')) => buffer.move_down(1)?,
                (M::NONE, Char('k')) => buffer.move_up(1)?,
                (M::NONE, Char('l')) => buffer.move_right(1)?,
                (M::NONE | M::SHIFT, Char('H')) => buffer.extend_left(1)?,
                (M::NONE | M::SHIFT, Char('J')) => buffer.extend_down(1)?,
                (M::NONE | M::SHIFT, Char('K')) => buffer.extend_up(1)?,
                (M::NONE | M::SHIFT, Char('L')) => buffer.extend_right(1)?,
                (M::NONE, Char(';')) => buffer.selection.reduce(),
                (M::ALT, Char(';')) => buffer.selection.flip(),
                (M::NONE, Char('d')) => buffer.delete()?,
                (M::NONE, Char('u')) => buffer.undo(),
                (M::NONE | M::SHIFT, Char('U')) => buffer.redo(),
                (M::NONE, Char('g')) => editor.mode = Mode::Goto,
                (M::NONE, Char('i')) => {
                    match insert_style {
                        InsertStyle::Flip => buffer.selection.flip_backward(),
                        InsertStyle::Stay => {}
                        InsertStyle::Reduce => buffer.selection.reduce(),
                    }
                    buffer.record();
                    editor.mode = Mode::Insert { after: false };
                }
                (M::NONE, Char('a')) => {
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
                (M::NONE, Up) => editor.scroll_up(1),
                (M::NONE, Down) => editor.scroll_down(1),
                (M::NONE, Left) => editor.scroll_left(1),
                (M::NONE, Right) => editor.scroll_right(1),
                (M::CONTROL, Char('d')) => {
                    editor.scroll_down(usize::from(area.height / 2));
                }
                (M::CONTROL, Char('u')) => {
                    editor.scroll_up(usize::from(area.height / 2));
                }
                (M::CONTROL, Char('s')) => editor.save()?,
                (M::CONTROL, Char('c')) => quit = true,
                (M::CONTROL, Char('p')) => panic!(),
                _ => {}
            },
            Event::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
                (M::ALT, MouseEventKind::ScrollUp) => editor.scroll_up(1),
                (M::ALT, MouseEventKind::ScrollDown) => editor.scroll_down(1),
                (M::NONE, MouseEventKind::ScrollUp) => editor.scroll_up(3),
                (M::NONE, MouseEventKind::ScrollDown) => editor.scroll_down(3),
                (M::SHIFT, MouseEventKind::ScrollUp) => editor.scroll_up(6),
                (M::SHIFT, MouseEventKind::ScrollDown) => editor.scroll_down(6),
                _ => {}
            },
            _ => {}
        },
        Mode::Goto => match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (M::NONE, Char('h')) => {
                    buffer.move_line_start()?;
                    editor.mode = Mode::Normal;
                }
                (M::NONE, Char('l')) => {
                    buffer.move_line_end()?;
                    editor.mode = Mode::Normal;
                }
                (M::NONE | M::SHIFT, Char('H')) => {
                    buffer.extend_line_start()?;
                    editor.mode = Mode::Normal;
                }
                (M::NONE | M::SHIFT, Char('L')) => {
                    buffer.extend_line_end()?;
                    editor.mode = Mode::Normal;
                }
                _ => {
                    editor.mode = Mode::Normal;
                }
            },
            _ => {}
        },
        Mode::Insert { after } => match event {
            Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
                (M::NONE, Char(char)) => buffer.insert_char(char)?,
                (M::SHIFT, Char(char)) => buffer.insert_char(char)?,
                (M::NONE, Enter) => buffer.insert_char('\n')?,
                (M::NONE, Backspace) => buffer.backspace()?,
                (M::CONTROL, Char('s')) => editor.save()?,
                (M::NONE, Esc) => {
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
        Mode::Normal => "normal",
        Mode::Goto => "goto",
        Mode::Insert { .. } => "insert",
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
