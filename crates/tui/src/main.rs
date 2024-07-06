mod terminal;

use crate::terminal::TerminalGuard;
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers, MouseEventKind};
use indigo_core::{actions, Cursor, CursorExt as _, Editor, RangeExt as _, RopeExt as _};
use ratatui::{
    prelude::{Buffer as Surface, Constraint, Layout, Rect, Style},
    style::Color,
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
        crossterm::queue!(
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
        let areas = Layout::vertical([
            // line_numbers + text
            Constraint::Fill(1),
            // status_bar
            Constraint::Length(1),
        ])
        .split(area);

        let status_bar = areas[1];

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
        .split(areas[0]);

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
    render_line_numbers(editor, areas.line_numbers, surface);
    render_tildes(editor, areas.line_numbers, surface);
    render_text(editor, areas.text, surface);
    render_range(editor, areas.text, surface);
    render_status_bar(editor, areas.status_bar, surface);
}

fn render_line_numbers(editor: &Editor, area: Rect, surface: &mut Surface) {
    let total_lines = editor.text().len_lines_indigo();

    let number_width = usize::from(area.width) - 1;

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y - area.top()) + editor.vertical_scroll() + 1;

        if line_number > total_lines {
            break;
        }

        surface.set_stringn(
            area.x,
            y,
            format!("{line_number:>number_width$}│"),
            number_width + 1,
            Style::default(),
        );
    }
}

fn render_tildes(editor: &Editor, area: Rect, surface: &mut Surface) {
    let total_lines = editor.text().len_lines_indigo();

    for y in area.top()..area.bottom() {
        let line_number = usize::from(y - area.top()) + editor.vertical_scroll() + 1;

        if line_number <= total_lines {
            continue;
        }

        surface.get_mut(area.x, y).set_char('~');
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

            match grapheme.get_char(0) {
                Some('\t') => {
                    surface.set_stringn(x, y, "        ", 8, Style::default());
                    x += 8;
                    continue 'grapheme;
                }
                // Rendering got messed up because somebody changed the width of `\n` from 0 to 1:
                // https://github.com/unicode-rs/unicode-width/pull/45
                //
                // Because we're rendering line by line, and `\n` indicates the line has ended, we
                // can `continue` to the next line immediately. Otherwise we'd want to render this
                // as a space (` `) or an indicator character something (like `:set list` in Vim).
                Some('\n') => continue 'line,
                _ => {}
            }

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

fn render_range(editor: &Editor, area: Rect, surface: &mut Surface) {
    let range = editor.range();
    let anchor = range.anchor();
    let head = range.head();

    for cursor in [anchor, head] {
        let char_index = cursor.char_index();

        let grapheme_index = cursor.grapheme_index();

        let line_index = editor.text().char_to_line(char_index);

        if line_index < editor.vertical_scroll() {
            return;
        }

        let y = area.top() + u16::try_from(line_index - editor.vertical_scroll()).unwrap();

        if y >= area.bottom() {
            return;
        }

        let line = editor.text().line(line_index);

        let line_char_index = editor.text().line_to_char(line_index);

        let mut g = Cursor::from_char_index(editor.text(), line_char_index)
            .unwrap()
            .grapheme_index();
        let mut x = usize::from(area.left());
        let mut width = 1;

        for grapheme in line.graphemes() {
            width = if let Some('\t') = grapheme.get_char(0) {
                8
            } else {
                grapheme.width()
            };

            if g >= grapheme_index {
                break;
            }

            if x >= usize::from(area.width) {
                return;
            }

            x += width;
            g += 1;
        }

        let x = u16::try_from(x).unwrap();

        for x in x..x + u16::try_from(width).unwrap() {
            surface.get_mut(x, y).set_bg(Color::Rgb(0xff, 0xd3, 0x3d));
        }
    }
}

fn render_status_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    let count = editor.count;

    let range = editor.range();

    let cursor = range.head();

    let char_index = cursor.char_index();

    let eof = char_index == editor.text().len_chars();

    let status_bar = if let Some(grapheme) = cursor.grapheme() {
        let chars = grapheme.chars().collect::<Vec<_>>();
        let display_width = grapheme.width();
        format!("count={count}, char_index={char_index}, eof={eof}, grapheme=\"{grapheme:?}\", chars={chars:?}, display_width={display_width}")
    } else {
        format!("count={count}, char_index={char_index}, eof={eof}, grapheme=n/a, chars=n/a, display_width=n/a")
    };

    surface.set_stringn(
        area.x,
        area.y,
        status_bar,
        usize::from(area.width),
        Style::default(),
    );
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
            (KeyModifiers::NONE, KeyCode::Char(c @ ('0'..='9'))) => {
                let n = usize::from(u8::try_from(c).unwrap() - b'0');
                editor.count = editor.count.saturating_mul(10);
                editor.count = editor.count.saturating_add(n);
            }
            (KeyModifiers::NONE, KeyCode::Esc) => editor.count = 0,
            (KeyModifiers::NONE, KeyCode::Backspace) => actions::backspace(editor),
            (KeyModifiers::NONE, KeyCode::Char('d')) => actions::delete(editor),
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
