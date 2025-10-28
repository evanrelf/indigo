use crate::{
    areas::{Areas, char_index_to_area, line_index_to_area},
    colors,
};
use indigo_core::prelude::*;
use ratatui::prelude::{Buffer as Surface, *};
use std::borrow::Cow;

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let areas = Areas::new(editor, area);
    render_navigation_bar(editor, areas.navigation_bar, surface);
    render_line_numbers(editor, areas.line_numbers, surface);
    render_tildes(editor, areas.line_numbers, surface);
    render_text(editor, areas.text, surface);
    render_selection(editor, areas.text, surface);
    render_command_bar(editor, areas.command_bar, surface);
    render_status_bar(editor, areas.status_bar, surface);
}

fn render_navigation_bar(_editor: &Editor, area: Rect, surface: &mut Surface) {
    Line::styled(" ", Modifier::UNDERLINED).render(area, surface);
}

fn render_line_numbers(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = &editor.buffer;

    let total_lines = buffer.text().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + buffer.vertical_scroll() + 1;

        if line_number > total_lines {
            break;
        }

        Line::raw(format!("{line_number}│"))
            .right_aligned()
            .render(row, surface);
    }
}

fn render_tildes(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = &editor.buffer;

    let total_lines = buffer.text().len_lines_indigo();

    for (i, row) in area.rows().enumerate() {
        let line_number = i + buffer.vertical_scroll() + 1;

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
    let buffer = &editor.buffer;

    let lines = buffer.text().lines_at(buffer.vertical_scroll());

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

    if let Some(bottom_row) = area.rows().next_back() {
        surface.set_style(bottom_row, Modifier::UNDERLINED);
    }
}

fn render_selection(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = &editor.buffer;

    let rope = buffer.text();

    let range = buffer.range();

    let vertical_scroll = buffer.vertical_scroll();

    let start_line = rope.char_to_line(range.start().char_offset());

    let end_line = rope.char_to_line(range.end().char_offset().saturating_sub(1));

    let grapheme_area = |char_index| char_index_to_area(char_index, rope, vertical_scroll, area);

    let line_area = |line_index| line_index_to_area(line_index, rope, vertical_scroll, area);

    if range.is_empty() {
        if let Some(rect) = grapheme_area(range.head().char_offset()) {
            surface.set_style(rect, Style::default().bg(colors::RED));
        }
        return;
    }

    for (line_index, mut line_rect) in (start_line..=end_line)
        .filter_map(|line_index| line_area(line_index).map(|rect| (line_index, rect)))
    {
        if line_index == start_line {
            if let Some(start_rect) = grapheme_area(range.start().char_offset()) {
                let delta = start_rect.x - line_rect.x;
                line_rect.x += delta;
                line_rect.width -= delta;
            } else {
                // TODO: We continue here because we know the range start is off the screen to the
                // right. Once horizontal scrolling is added, we'll need to handle when the range is
                // off the screen to the left. `grapheme_area` doesn't say which direction the index
                // is off screen.
                continue;
            }
        }
        #[expect(clippy::collapsible_if)]
        if line_index == end_line {
            if let Some(end_rect) = grapheme_area(range.end().char_offset().saturating_sub(1)) {
                let delta = line_rect.right() - end_rect.right();
                line_rect.width -= delta;
            }
        }
        surface.set_style(line_rect, Style::default().bg(colors::LIGHT_YELLOW));
    }

    #[expect(clippy::collapsible_else_if)]
    if range.is_backward() {
        if let Some(rect) = grapheme_area(range.head().char_offset()) {
            surface.set_style(rect, Style::default().bg(colors::DARK_YELLOW));
        }
    } else {
        if let Some(rect) = grapheme_area(range.head().char_offset().saturating_sub(1)) {
            surface.set_style(rect, Style::default().bg(colors::DARK_YELLOW));
        }
    }
}

fn render_command_bar(editor: &Editor, mut area: Rect, surface: &mut Surface) {
    let Mode::Command(ref normal_mode) = editor.mode else {
        return;
    };

    if let Some(cell) = surface.cell_mut(area.as_position()) {
        cell.set_char(':');
    } else {
        unreachable!();
    }

    area.x += 1;
    area.width -= 1;

    Line::raw(Cow::<str>::from(normal_mode.text().rope())).render(area, surface);

    if let Some(rect) = char_index_to_area(
        normal_mode.cursor().char_offset(),
        normal_mode.text(),
        0,
        area,
    ) {
        surface.set_style(rect, Style::default().bg(colors::DARK_YELLOW));
    }
}

fn render_status_bar(editor: &Editor, area: Rect, surface: &mut Surface) {
    let buffer = &editor.buffer;

    let range = buffer.range();

    let anchor = range.anchor().char_offset();

    let head = range.head().char_offset();

    let char_length = range.char_length();

    let grapheme_length = range.grapheme_length();

    let display_width = range.slice().display_width();

    let eof = range.is_eof();

    let mode = match editor.mode {
        Mode::Normal(_) => "normal",
        Mode::Insert(_) => "insert",
        Mode::Command(_) => "command",
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
