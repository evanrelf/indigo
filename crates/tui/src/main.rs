mod event;
mod key;
mod terminal;

use crate::terminal::TerminalGuard;
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{Event, KeyCode, KeyModifiers, MouseButton, MouseEventKind};
use indigo_core::{actions, prelude::*};
use ratatui::{
    prelude::{Buffer as Surface, Constraint, Layout, Position, Rect, Style, Widget as _},
    style::{Color, Modifier},
    text::Line,
};
use ropey::Rope;
use std::{borrow::Cow, cmp::max};

#[derive(Debug, clap::Parser)]
struct Args {
    file: Option<Utf8PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut editor = Editor::new(args.file)?;

    let mut terminal = terminal::enter()?;

    let mut areas = Areas::default();

    loop {
        terminal.draw(|frame| {
            areas = Areas::new(&editor, frame.area());
            editor.height = usize::from(areas.text.height);
            let surface = frame.buffer_mut();
            render(&editor, areas, surface);
        })?;

        let event = crossterm::event::read()?;

        handle_event(&mut editor, &mut terminal, areas, &event)?;

        if editor.quit {
            break;
        }
    }

    Ok(())
}

#[derive(Clone, Copy, Default)]
struct Areas {
    navigation_bar: Rect,
    line_numbers: Rect,
    text: Rect,
    status_bar: Rect,
}

impl Areas {
    fn new(editor: &Editor, area: Rect) -> Self {
        let vertical_areas = Layout::vertical([
            // navigation_bar
            Constraint::Length(1),
            // line_numbers + text
            Constraint::Fill(1),
            // status_bar
            Constraint::Length(1),
        ])
        .split(area);

        let line_numbers_width = {
            let n = editor.rope().len_lines_indigo();
            let digits = 1 + max(1, n).ilog10();
            u16::try_from(max(2, digits) + 1)
                .expect("Line number width should always be very small")
        };

        let navigation_bar = vertical_areas[0];

        let horizontal_areas = Layout::horizontal([
            // line_numbers
            Constraint::Length(line_numbers_width),
            // text
            Constraint::Fill(1),
        ])
        .split(vertical_areas[1]);

        let status_bar = vertical_areas[2];

        let line_numbers = horizontal_areas[0];

        let text = horizontal_areas[1];

        Self {
            navigation_bar,
            line_numbers,
            text,
            status_bar,
        }
    }
}

fn render(editor: &Editor, areas: Areas, surface: &mut Surface) {
    render_navigation_bar(editor, areas.navigation_bar, surface);
    render_line_numbers(editor, areas.line_numbers, surface);
    render_tildes(editor, areas.line_numbers, surface);
    render_text(editor, areas.text, surface);
    render_selection(editor, areas.text, surface);
    render_status_bar(editor, areas.status_bar, surface);
}

fn render_navigation_bar(_editor: &Editor, area: Rect, surface: &mut Surface) {
    Line::raw(" ")
        .style(Style::new().add_modifier(Modifier::UNDERLINED))
        .render(area, surface);
}

fn render_line_numbers(editor: &Editor, area: Rect, surface: &mut Surface) {
    let total_lines = editor.rope().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + editor.vertical_scroll() + 1;

        if line_number > total_lines {
            break;
        }

        Line::raw(format!("{line_number}│"))
            .right_aligned()
            .render(row, surface);
    }
}

fn render_tildes(editor: &Editor, area: Rect, surface: &mut Surface) {
    let total_lines = editor.rope().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + editor.vertical_scroll() + 1;

        if line_number <= total_lines {
            continue;
        }

        let bottom = row.y + 1 == area.bottom();

        Line::raw("~")
            .style(Style::new().add_modifier(if bottom {
                Modifier::UNDERLINED
            } else {
                Modifier::empty()
            }))
            .render(row, surface);
    }
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    let lines = editor.rope().lines_at(editor.vertical_scroll());

    let rows = area.rows();

    'line: for (line, row) in lines.zip(rows) {
        let mut x = row.x;

        'grapheme: for grapheme in line.graphemes() {
            let string = match grapheme.get_char(0) {
                Some('\t') => Cow::Borrowed("        "),
                Some('\n') => Cow::Borrowed(" "),
                _ => Cow::<str>::from(grapheme),
            };

            let width_usize = grapheme.display_width();

            let width_u16 = u16::try_from(width_usize).unwrap();

            if x + width_u16 > row.right() {
                continue 'line;
            }

            if width_usize == 0 {
                continue 'grapheme;
            }

            surface.set_stringn(x, row.y, string, width_usize, Style::default());

            x += width_u16;
        }
    }

    if let Some(bottom_row) = area.rows().last() {
        surface.set_style(bottom_row, Style::new().add_modifier(Modifier::UNDERLINED));
    }
}

fn render_selection(editor: &Editor, area: Rect, surface: &mut Surface) {
    let range = editor.range();

    let cursor_area =
        |index: usize| char_index_to_area(index, editor.rope(), editor.vertical_scroll(), area);

    let light_yellow = Color::Rgb(0xff, 0xf5, 0xb1);
    let dark_yellow = Color::Rgb(0xff, 0xd3, 0x3d);
    let red = Color::Rgb(0xd7, 0x3a, 0x4a);
    let head_color = if range.head.is_eof(editor.rope()) {
        red
    } else {
        dark_yellow
    };

    for index in range.start()..=range.end() {
        if let Some(rect) = cursor_area(index) {
            let color = if index == range.head() {
                head_color
            } else {
                light_yellow
            };
            surface.set_style(rect, Style::default().bg(color));
        }
    }
}

/// Map a rope index to an area of the terminal. Example use is rendering the cell(s) where a cursor
/// sits in a different color.
fn char_index_to_area(
    char_index: usize,
    rope: &Rope,
    vertical_scroll: usize,
    area: Rect,
) -> Option<Rect> {
    let line_index = rope.try_char_to_line(char_index).ok()?;

    if vertical_scroll > line_index {
        return None;
    }

    let line_char_index = rope.try_line_to_char(line_index).ok()?;

    let prefix_width = rope.get_slice(line_char_index..char_index)?.display_width();

    let x = area.x + u16::try_from(prefix_width).unwrap();

    let y = area.y + u16::try_from(line_index - vertical_scroll).unwrap();

    let width = match rope.get_grapheme(char_index) {
        Some(grapheme) => u16::try_from(grapheme.display_width()).unwrap(),
        None => 1, // Cursor at end of file
    };

    Some(Rect {
        x,
        y,
        width,
        height: 1,
    })
}

/// Map position on the terminal to a character index in the rope indices. Example us is moving a
/// cursor to where a mouse was clicked.
///
/// `None` means the position was not contained within the area. `Some(Ok(_))` means the position
/// was valid in the rope. `Some(Err(_))` means the position was not valid in the rope, but we were
/// able to correct it.
///
/// Examples of corrections: snapping to the beginning of the grapheme, snapping to the end of the
/// line, and snapping to the end of the buffer.
fn position_to_char_index(
    position: Position,
    rope: &Rope,
    vertical_scroll: usize,
    area: Rect,
) -> Option<Result<usize, usize>> {
    // TODO: Move this general purpose (x, y) <-> index logic somewhere else.

    if !area.contains(position) {
        return None;
    }

    let x = usize::from(position.x - area.x);

    let y = usize::from(position.y - area.y) + vertical_scroll;

    let Some(line) = rope.get_line(y) else {
        // Position goes beyond last line of rope, so we snap to last character of rope
        return Some(Err(rope.len_chars()));
    };

    let line_char_index = rope
        .try_line_to_char(y)
        .expect("Line is known to exist at this point");

    let line_length = line.len_chars();

    if x > line_length {
        // Position goes beyond last character of line, so we snap to last character of line
        return Some(Err(line_char_index + (line_length - 1)));
    }

    Some(Ok(line_char_index + x))
}

fn render_status_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    let anchor = editor.range().anchor();

    let head = editor.range().head();

    let mode = match editor.mode {
        Mode::Normal { .. } => "normal",
        Mode::Insert => "insert",
    };

    let count = editor.mode.count();

    let eof = editor.range().head() == editor.rope().len_chars();

    let status_bar = format!("anchor={anchor} head={head} mode={mode} count={count} eof={eof}");

    Line::raw(status_bar).render(area, surface);
}

fn handle_event(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<()> {
    match editor.mode {
        Mode::Normal(_) => handle_event_normal(editor, terminal, areas, event),
        Mode::Insert => handle_event_insert(editor, terminal, areas, event),
    }
}

fn handle_event_normal(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<()> {
    let Mode::Normal(ref mut normal_mode) = editor.mode else {
        unreachable!()
    };

    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Char(c @ ('0'..='9'))) => {
                let n = usize::from(u8::try_from(c).unwrap() - b'0');
                normal_mode.count = normal_mode.count.saturating_mul(10).saturating_add(n);
            }
            (KeyModifiers::NONE, KeyCode::Esc) => actions::enter_normal_mode(editor),
            (KeyModifiers::NONE, KeyCode::Char('i')) => actions::enter_insert_mode(editor),
            // TODO: Add `a` for entering insert mode with the cursor moved to the right.
            (KeyModifiers::NONE, KeyCode::Char('h')) => actions::move_left(editor),
            (KeyModifiers::NONE, KeyCode::Char('l')) => actions::move_right(editor),
            (KeyModifiers::SHIFT, KeyCode::Char('h' | 'H')) => actions::extend_left(editor),
            (KeyModifiers::SHIFT, KeyCode::Char('l' | 'L')) => actions::extend_right(editor),
            (KeyModifiers::NONE, KeyCode::Char(';')) => actions::reduce(editor),
            (KeyModifiers::ALT, KeyCode::Char(';')) => actions::flip(editor),
            (ms, KeyCode::Char(';')) if ms == KeyModifiers::ALT | KeyModifiers::SHIFT => {
                actions::flip_forward(editor);
            }
            (KeyModifiers::NONE, KeyCode::Char('d')) => actions::delete(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('u')) => actions::scroll_half_page_up(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('d')) => actions::scroll_half_page_down(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('b')) => actions::scroll_full_page_up(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('f')) => actions::scroll_full_page_down(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => editor.quit = true,
            _ => {}
        },
        Event::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => actions::scroll_up(editor),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => actions::scroll_down(editor),
            // TODO: Kakoune allows creating new selection ranges by control clicking. Would be
            // awesome if Indigo could do the same, but also support control dragging to create
            // vertical lines of selection ranges, akin to Vim's visual block mode. Could snap to
            // the same column? Might be weird in the presence of wide characters.
            (KeyModifiers::NONE, MouseEventKind::Down(MouseButton::Left)) => {
                let position = Position {
                    x: mouse_event.column,
                    y: mouse_event.row,
                };
                if let Some(Err(index) | Ok(index)) = position_to_char_index(
                    position,
                    editor.rope(),
                    editor.vertical_scroll(),
                    areas.text,
                ) {
                    actions::move_to(editor, index);
                }
            }
            (KeyModifiers::NONE, MouseEventKind::Down(MouseButton::Right)) => {
                let position = Position {
                    x: mouse_event.column,
                    y: mouse_event.row,
                };
                if let Some(Err(index) | Ok(index)) = position_to_char_index(
                    position,
                    editor.rope(),
                    editor.vertical_scroll(),
                    areas.text,
                ) {
                    actions::extend_to(editor, index);
                }
            }
            _ => {}
        },
        _ => {}
    }

    Ok(())
}

fn handle_event_insert(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    _areas: Areas,
    event: &Event,
) -> anyhow::Result<()> {
    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Esc) => actions::enter_normal_mode(editor),
            (KeyModifiers::NONE, KeyCode::Backspace) => actions::delete_before(editor),
            (KeyModifiers::NONE, KeyCode::Delete) => actions::delete_after(editor),
            (KeyModifiers::NONE, KeyCode::Char(c)) => actions::insert_char(editor, c),
            (KeyModifiers::NONE, KeyCode::Enter) => actions::insert_char(editor, '\n'),
            (KeyModifiers::NONE, KeyCode::Tab) => actions::insert_char(editor, '\t'),
            (KeyModifiers::CONTROL, KeyCode::Char('u')) => actions::scroll_half_page_up(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('d')) => actions::scroll_half_page_down(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('b')) => actions::scroll_full_page_up(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('f')) => actions::scroll_full_page_down(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => editor.quit = true,
            _ => {}
        },
        Event::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => actions::scroll_up(editor),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => actions::scroll_down(editor),
            _ => {}
        },
        Event::Paste(string) => actions::insert(editor, string),
        _ => {}
    }

    Ok(())
}
