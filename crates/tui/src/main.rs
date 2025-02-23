mod areas;
mod event;
mod key;
mod terminal;

use crate::{areas::Areas, event::handle_event};
use camino::Utf8PathBuf;
use clap::Parser as _;
use indigo_core::prelude::*;
use ratatui::{
    prelude::{Buffer as Surface, Rect, Style, Widget as _},
    style::{Color, Modifier},
    text::{Line, Span},
};
use std::{fs::File, io::BufReader};

#[derive(Debug, clap::Parser)]
struct Args {
    file: Option<Utf8PathBuf>,
}

fn main() -> anyhow::Result<()> {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        // SAFETY: At this point the program is single-threaded. There are no other threads that
        // could be reading from or writing to the environment.
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }
    }

    let args = Args::parse();

    let rope = if let Some(path) = args.file {
        let file = File::open(path)?;
        Rope::from_reader(BufReader::new(file))?
    } else {
        Rope::new()
    };

    let mut editor = Editor::from_rope(rope);

    let mut terminal = terminal::enter()?;

    let mut areas = Areas::default();

    loop {
        #[cfg(feature = "coz")]
        coz::begin!("tui:draw");
        terminal.draw(|frame| {
            areas = Areas::new(&editor, frame.area());
            editor.height = usize::from(areas.text.height);
            let surface = frame.buffer_mut();
            render(&editor, areas, surface);
        })?;
        #[cfg(feature = "coz")]
        coz::end!("tui:draw");

        let event = crossterm::event::read()?;

        handle_event(&mut editor, &mut terminal, areas, &event)?;

        if editor.quit {
            break;
        }
    }

    Ok(())
}

fn render(editor: &Editor, areas: Areas, surface: &mut Surface) {
    #[cfg(feature = "coz")]
    coz::scope!("tui:render");

    render_navigation_bar(editor, areas.navigation_bar, surface);
    render_line_numbers(editor, areas.line_numbers, surface);
    render_tildes(editor, areas.line_numbers, surface);
    render_text(editor, areas.text, surface);
    render_selection(editor, areas.text, surface);
    render_status_bar(editor, areas.status_bar, surface);
}

fn render_navigation_bar(_editor: &Editor, area: Rect, surface: &mut Surface) {
    #[cfg(feature = "coz")]
    coz::scope!("tui:render_navigation_bar");

    Line::styled(" ", Modifier::UNDERLINED).render(area, surface);
}

fn render_line_numbers(editor: &Editor, area: Rect, surface: &mut Surface) {
    #[cfg(feature = "coz")]
    coz::scope!("tui:render_line_numbers");

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
    #[cfg(feature = "coz")]
    coz::scope!("tui:render_tildes");

    let total_lines = editor.rope().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + editor.vertical_scroll() + 1;

        if line_number <= total_lines {
            continue;
        }

        let bottom = row.y + 1 == area.bottom();

        let style = if bottom {
            Modifier::UNDERLINED
        } else {
            Modifier::empty()
        };

        Line::styled("~", style).render(row, surface);
    }
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    #[cfg(feature = "coz")]
    coz::scope!("tui:render_text");

    let lines = editor.rope().lines_at(editor.vertical_scroll());

    let rows = area.rows();

    'line: for (line, mut rect) in lines.zip(rows) {
        'grapheme: for grapheme in line.graphemes() {
            let span = match grapheme.get_char(0) {
                Some('\t') => Span::styled("→       ", Color::Rgb(0xee, 0xee, 0xee)),
                Some('\n') => Span::styled("¬", Color::Rgb(0xee, 0xee, 0xee)),
                _ => Span::raw(grapheme),
            };

            let width_usize = grapheme.display_width();

            let width_u16 = u16::try_from(width_usize).unwrap();

            if rect.x + width_u16 > rect.right() {
                continue 'line;
            }

            if width_usize == 0 {
                continue 'grapheme;
            }

            span.render(rect, surface);

            rect.x += width_u16;
        }
    }

    if let Some(bottom_row) = area.rows().last() {
        surface.set_style(bottom_row, Modifier::UNDERLINED);
    }
}

fn render_selection(editor: &Editor, area: Rect, surface: &mut Surface) {
    #[cfg(feature = "coz")]
    coz::scope!("tui:render_selection");

    let range = editor.range();

    let cursor_area =
        |index: usize| char_index_to_area(index, editor.rope(), editor.vertical_scroll(), area);

    let light_yellow = Color::Rgb(0xff, 0xf5, 0xb1);
    let dark_yellow = Color::Rgb(0xff, 0xd3, 0x3d);
    let red = Color::Rgb(0xd7, 0x3a, 0x4a);

    for rect in (range.start()..range.end()).filter_map(cursor_area) {
        surface.set_style(rect, Style::default().bg(light_yellow));
    }

    #[expect(clippy::collapsible_else_if)]
    if range.is_empty() {
        if let Some(rect) = cursor_area(range.head()) {
            surface.set_style(rect, Style::default().bg(red));
        }
    } else if range.is_backward() {
        if let Some(rect) = cursor_area(range.head()) {
            surface.set_style(rect, Style::default().bg(dark_yellow));
        }
    } else {
        if let Some(rect) = cursor_area(range.head().saturating_sub(1)) {
            surface.set_style(rect, Style::default().bg(dark_yellow));
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
    let char_index = rope.snap_to_grapheme_boundary(char_index, Bias::Before);

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

fn render_status_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    #[cfg(feature = "coz")]
    coz::scope!("tui:render_status_bar");

    let anchor = editor.range().anchor();

    let head = editor.range().head();

    let char_length = editor.range().char_length();

    let grapheme_length = editor.range().grapheme_length(editor.rope());

    let display_width = editor.range().rope_slice(editor.rope()).display_width();

    let eof = editor.range().head() == editor.rope().len_chars();

    let mode = match editor.mode {
        Mode::Normal { .. } => "normal",
        Mode::Insert => "insert",
    };

    let count = editor.mode.count();

    let status_bar = [
        format!("anchor={anchor}"),
        format!("head={head}"),
        format!("char_length={char_length}"),
        format!("grapheme_length={grapheme_length}"),
        format!("display_width={display_width}"),
        format!("eof={eof}"),
        format!("mode={mode}"),
        format!("count={count}"),
    ]
    .join(" ");

    Line::raw(status_bar).render(area, surface);
}
