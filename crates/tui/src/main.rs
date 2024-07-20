mod terminal;

use crate::terminal::TerminalGuard;
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use indigo_core::{
    actions, Cursor, CursorExt as _, DisplayWidth as _, Editor, Mode, RangeExt as _, RopeExt as _,
};
use ratatui::{
    prelude::{Buffer as Surface, Constraint, Layout, Rect, Style, Widget as _},
    style::Color,
    text::Line,
};
use std::{borrow::Cow, cmp::max};

#[derive(Debug, clap::Parser)]
struct Args {
    file: Utf8PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut editor = Editor::new(args.file)?;

    let mut terminal = terminal::enter()?;

    let mut areas = Areas::default();

    loop {
        crossterm::execute!(
            terminal.backend_mut(),
            crossterm::terminal::BeginSynchronizedUpdate
        )?;

        terminal.draw(|frame| {
            areas = Areas::new(&editor, frame.size());
            let surface = frame.buffer_mut();
            render(&editor, areas, surface);
        })?;

        crossterm::execute!(
            terminal.backend_mut(),
            crossterm::terminal::EndSynchronizedUpdate
        )?;

        let quit = handle_event(&mut editor, &mut terminal, areas, &event::read()?)?;

        if quit {
            break;
        }
    }

    Ok(())
}

#[derive(Clone, Copy, Default)]
struct Areas {
    status_bar: Rect,
    line_numbers: Rect,
    text: Rect,
}

impl Areas {
    fn new(editor: &Editor, area: Rect) -> Self {
        let vertical_areas = Layout::vertical([
            // line_numbers + text
            Constraint::Fill(1),
            // status_bar
            Constraint::Length(1),
        ])
        .split(area);

        let status_bar = vertical_areas[1];

        let line_numbers_width = {
            let n = editor.text().len_lines_indigo();
            let digits = 1 + max(1, n).ilog10();
            u16::try_from(max(2, digits) + 1).unwrap()
        };

        let horizontal_areas = Layout::horizontal([
            // line_numbers
            Constraint::Length(line_numbers_width),
            // text
            Constraint::Fill(1),
        ])
        .split(vertical_areas[0]);

        let line_numbers = horizontal_areas[0];

        let text = horizontal_areas[1];

        Self {
            status_bar,
            line_numbers,
            text,
        }
    }
}

fn render(editor: &Editor, areas: Areas, surface: &mut Surface) {
    render_line_numbers(editor, areas.line_numbers, surface);
    render_tildes(editor, areas.line_numbers, surface);
    render_text(editor, areas.text, surface);
    render_selection(editor, areas.text, surface);
    render_status_bar(editor, areas.status_bar, surface);
}

fn render_line_numbers(editor: &Editor, area: Rect, surface: &mut Surface) {
    let total_lines = editor.text().len_lines_indigo();

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
    let total_lines = editor.text().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + editor.vertical_scroll() + 1;

        if line_number <= total_lines {
            continue;
        }

        Line::raw("~").render(row, surface);
    }
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    let lines = editor.text().lines_at(editor.vertical_scroll());

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
}

fn render_selection(editor: &Editor, area: Rect, surface: &mut Surface) {
    let range = editor.range();

    if let Some(anchor_rect) = cursor_area(range.anchor(), editor, area) {
        let anchor_style = Style::default().bg(Color::Rgb(0xff, 0xf5, 0xb1));
        surface.set_style(anchor_rect, anchor_style);
    }

    if let Some(head_rect) = cursor_area(range.head(), editor, area) {
        let head_style = Style::default().bg(Color::Rgb(0xff, 0xd3, 0x3d));
        surface.set_style(head_rect, head_style);
    }
}

fn cursor_area(cursor: Cursor, editor: &Editor, area: Rect) -> Option<Rect> {
    let char_index = cursor.char_index();

    let line_index = editor.text().char_to_line(char_index);

    if editor.vertical_scroll() > line_index {
        return None;
    }

    let line_char_index = editor.text().line_to_char(line_index);

    let prefix_width = editor
        .text()
        .slice(line_char_index..char_index)
        .display_width();

    let x = area.x + u16::try_from(prefix_width).unwrap();

    let y = area.y + u16::try_from(line_index - editor.vertical_scroll()).unwrap();

    let width = match cursor.grapheme() {
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

fn render_status_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    let mode = editor.mode();

    let count = editor.count;

    let range = editor.range();

    let cursor = range.head();

    let char_index = cursor.char_index();

    let eof = char_index == editor.text().len_chars();

    let status_bar = if let Some(grapheme) = cursor.grapheme() {
        let chars = grapheme.chars().collect::<Vec<_>>();
        let display_width = grapheme.display_width();
        format!("mode={mode}, count={count}, char_index={char_index}, eof={eof}, grapheme=\"{grapheme:?}\", chars={chars:?}, display_width={display_width}")
    } else {
        format!("mode={mode}, count={count}, char_index={char_index}, eof={eof}, grapheme=n/a, chars=n/a, display_width=n/a")
    };

    Line::raw(status_bar).render(area, surface);
}

fn handle_event(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<bool> {
    match editor.mode() {
        Mode::Normal => handle_event_normal(editor, terminal, areas, event),
        Mode::Insert => handle_event_insert(editor, terminal, areas, event),
    }
}

fn handle_event_normal(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<bool> {
    let mut quit = false;

    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Char(c @ ('0'..='9'))) => {
                let n = usize::from(u8::try_from(c).unwrap() - b'0');
                editor.count = editor.count.saturating_mul(10);
                editor.count = editor.count.saturating_add(n);
            }
            (KeyModifiers::NONE, KeyCode::Esc) => editor.count = 0,
            (KeyModifiers::NONE, KeyCode::Char('h')) => actions::move_left(editor),
            (KeyModifiers::NONE, KeyCode::Char('l')) => actions::move_right(editor),
            (KeyModifiers::SHIFT, KeyCode::Char('h' | 'H')) => actions::extend_left(editor),
            (KeyModifiers::SHIFT, KeyCode::Char('l' | 'L')) => actions::extend_right(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('u')) => {
                actions::scroll_half_page_up(editor, usize::from(areas.text.height));
            }
            (KeyModifiers::CONTROL, KeyCode::Char('d')) => {
                actions::scroll_half_page_down(editor, usize::from(areas.text.height));
            }
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => quit = true,
            _ => {}
        },
        Event::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => actions::scroll_up(editor),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => actions::scroll_down(editor),
            _ => {}
        },
        _ => {}
    }

    Ok(quit)
}

fn handle_event_insert(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<bool> {
    let mut quit = false;

    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Backspace) => actions::backspace(editor),
            (KeyModifiers::NONE, KeyCode::Char('d')) => actions::delete(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('u')) => {
                actions::scroll_half_page_up(editor, usize::from(areas.text.height));
            }
            (KeyModifiers::CONTROL, KeyCode::Char('d')) => {
                actions::scroll_half_page_down(editor, usize::from(areas.text.height));
            }
            (KeyModifiers::CONTROL, KeyCode::Char('l')) => terminal.clear()?,
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => quit = true,
            _ => {}
        },
        Event::Mouse(mouse_event) => match (mouse_event.modifiers, mouse_event.kind) {
            (KeyModifiers::NONE, MouseEventKind::ScrollUp) => actions::scroll_up(editor),
            (KeyModifiers::NONE, MouseEventKind::ScrollDown) => actions::scroll_down(editor),
            _ => {}
        },
        _ => {}
    }

    Ok(quit)
}
