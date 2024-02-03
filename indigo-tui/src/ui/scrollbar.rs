use crate::ui::colors::GRAY_300;
use indigo_core::{Editor, RopeExt};
use ratatui::prelude::{Buffer as Surface, *};
use std::cmp::min;

// TODO: Use Unicode box drawing characters to render at a sub-cell level for smoother scrolling

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    if area.width == 0 || area.height < 1 {
        return;
    }

    let color = GRAY_300;

    let window = editor.current_window();

    let buffer = window.file().buffer();

    let total_lines = buffer.contents().len_lines_indigo();

    let scrollbar_gutter = usize::from(area.height);

    let scrollbar_size =
        (min(usize::from(area.height), total_lines) * scrollbar_gutter) / total_lines;

    let scrollbar_scroll =
        (window.vertical_scroll() * (scrollbar_gutter - scrollbar_size)) / total_lines;

    let start = usize::from(area.top()) + scrollbar_scroll;

    let end = start + scrollbar_size;

    // Scrollbar
    for y in start..=end {
        let x = area.right();
        let y = u16::try_from(y).unwrap();
        surface.get_mut(x, y).set_char('▐').set_fg(color);
    }
}
