use crate::ui::colors::{BG_GRAY, BG_YELLOW};
use indigo_core::{CommandMode, Editor, Mode};
use ratatui::prelude::{Buffer as Surface, *};
use std::cmp::min;

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let Mode::Command(command_mode) = editor.mode() else {
        return;
    };
    render_command(command_mode, area, surface);
    render_cursor(command_mode, area, surface);
}

fn render_command(command_mode: &CommandMode, area: Rect, surface: &mut Surface) {
    surface
        .get_mut(area.x, area.y)
        .set_char(':')
        .set_fg(Color::Reset);

    let rope = command_mode.command();

    if rope.len_chars() == 0 {
        return;
    }

    for y in area.top()..area.bottom() {
        let i = y - area.top();

        let start = usize::from(area.width * i).saturating_sub(1);
        let end = usize::from(area.width * (i + 1)).saturating_sub(1);
        let range = start..min(rope.len_chars(), end);

        // Don't do all the rest if the string is empty
        if range.is_empty() {
            break;
        }

        let mut line = rope.slice(range).to_string();
        line.push(' ');

        // Offset the first line to account for the `:`
        let x = area.x + if i == 0 { 1 } else { 0 };

        surface.set_string(x, y, line, Style::default().fg(Color::Reset).bg(BG_GRAY));
    }
}

fn render_cursor(command_mode: &CommandMode, area: Rect, surface: &mut Surface) {
    let cursor = command_mode.cursor() + 1;

    let x = u16::try_from(usize::from(area.x) + (cursor % usize::from(area.width))).unwrap();
    let y = u16::try_from(usize::from(area.y) + (cursor / usize::from(area.width))).unwrap();

    surface.get_mut(x, y).set_bg(BG_YELLOW);
}

pub fn lines(command_mode: &CommandMode, area: Rect) -> u16 {
    let chars = command_mode.command().len_chars() + 1;
    let lines = (chars / usize::from(area.width)) + 1;
    u16::try_from(lines).unwrap()
}
