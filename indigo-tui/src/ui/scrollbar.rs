use crate::ui::colors::GRAY_300;
use indigo_core::{Editor, RopeExt};
use ratatui::prelude::{Buffer as Surface, *};
use std::cmp::min;

// TODO: Use Unicode box drawing characters to render at a sub-cell level for smoother scrolling

// TODO: Only let scrollbar touch the top when `vertical_scroll` is 0

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    if area.width == 0 || area.height == 0 {
        return;
    }

    let window = editor.current_window();

    let buffer = window.file().buffer();

    let total_lines = buffer.contents().len_lines_indigo();

    // Assumes scrollbar area has same height as buffer area
    let scrollbar_gutter = usize::from(area.height);

    // Don't show scrollbar if there's no need to scroll
    if scrollbar_gutter >= total_lines {
        return;
    }

    let scrollbar_size = (min(scrollbar_gutter, total_lines) * scrollbar_gutter) / total_lines;

    // Don't show scrollbar if it'll never move
    if scrollbar_size == scrollbar_gutter {
        return;
    }

    let scrollbar_scroll =
        (window.vertical_scroll() * (scrollbar_gutter - scrollbar_size)) / total_lines;

    // TODO: rem_euclid

    let start = usize::from(area.top()) + scrollbar_scroll;

    let end = start + scrollbar_size;

    let color = GRAY_300;

    let x = area.right() - 1;

    for y in start..=end {
        let y = u16::try_from(y).unwrap();
        surface.get_mut(x, y).set_char('▐').set_fg(color);
    }
}
