mod terminal;

use crate::terminal::TerminalGuard;
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use indigo_core::{actions, CursorExt as _, Editor, RopeExt as _};
use ratatui::prelude::{Buffer as Surface, Constraint, Layout, Rect, Style};
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
        terminal.draw(|frame| {
            areas = Areas::new(&editor, frame.size());
            let surface = frame.buffer_mut();
            render(&editor, areas, surface);
        })?;

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
        let areas = Layout::vertical([
            // status_bar
            Constraint::Length(1),
            // line_numbers + text
            Constraint::Fill(1),
        ])
        .split(area);

        let status_bar = areas[0];

        let line_numbers_width = {
            let n = editor.text().len_lines_indigo();
            let digits = 1 + max(1, n).ilog10();
            u16::try_from(max(2, digits) + 1).unwrap()
        };

        let areas = Layout::horizontal([
            // line_numbers
            Constraint::Length(line_numbers_width),
            // text
            Constraint::Fill(1),
        ])
        .split(areas[1]);

        let line_numbers = areas[0];

        let text = areas[1];

        Self {
            status_bar,
            line_numbers,
            text,
        }
    }
}

fn render(editor: &Editor, areas: Areas, surface: &mut Surface) {
    render_status_bar(editor, areas.status_bar, surface);
    render_line_numbers(editor, areas.line_numbers, surface);
    render_text(editor, areas.text, surface);
}

fn render_status_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    let char_index = editor.cursor().char_index();

    let eof = char_index == editor.text().len_chars();

    let slice = editor.text().slice(char_index..);

    let status_bar = if let Some(grapheme) = slice.graphemes().next() {
        let chars = grapheme.chars().count();
        let width = grapheme.width();
        format!("char_index={char_index}, eof={eof}, grapheme=\"{grapheme}\", chars={chars}, width={width}")
    } else {
        format!("char_index={char_index}, eof={eof}, grapheme=n/a, chars=n/a, width=n/a")
    };

    surface.set_stringn(
        area.x,
        area.y,
        status_bar,
        usize::from(area.width),
        Style::default(),
    );
}

fn render_line_numbers(editor: &Editor, area: Rect, surface: &mut Surface) {
    let total_lines = editor.text().len_lines_indigo();

    let number_width = usize::from(area.width) - 1;

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y) + editor.vertical_scroll();

        if line_number <= total_lines {
            surface.set_stringn(
                area.x,
                y,
                format!("{line_number:>number_width$}│"),
                number_width + 1,
                Style::default(),
            );
        }
    }
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    'line: for (y, line) in editor.text().lines_at(editor.vertical_scroll()).enumerate() {
        let y = area.top() + u16::try_from(y).unwrap();

        if y >= area.bottom() {
            break 'line;
        }

        let mut x = area.x;

        'grapheme: for grapheme in line.graphemes() {
            if x >= area.width {
                continue 'line;
            }

            if let Some('\t') = grapheme.get_char(0) {
                surface.set_stringn(x, y, "        ", 8, Style::default());
                x += 8;
                continue 'grapheme;
            };

            let width = grapheme.width();

            if width == 0 {
                continue 'grapheme;
            }

            let string = Cow::<str>::from(grapheme);

            surface.set_stringn(x, y, string, width, Style::default());

            x += u16::try_from(width).unwrap();
        }
    }
}

fn handle_event(
    editor: &mut Editor,
    terminal: &mut TerminalGuard,
    areas: Areas,
    event: &Event,
) -> anyhow::Result<bool> {
    let mut quit = false;

    #[allow(clippy::single_match)]
    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Backspace) => actions::backspace(editor),
            (KeyModifiers::NONE, KeyCode::Char('d')) => actions::delete(editor),
            (KeyModifiers::NONE, KeyCode::Char('h')) => actions::move_left(editor),
            (KeyModifiers::NONE, KeyCode::Char('l')) => actions::move_right(editor),
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
