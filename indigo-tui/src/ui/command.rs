use crate::ui::colors::YELLOW;
use indigo_core::{Editor, Mode};
use ratatui::prelude::{Buffer as Surface, *};

// TODO: Scroll (instead of panicking) when command length exceeds area width
// TODO: Paint over status elements a few characters ahead of the command, so it's clear the command
// text isn't part of the status

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let Mode::Command(command_mode) = editor.mode() else {
        return;
    };

    surface.set_string(
        area.x,
        area.y,
        format!(":{}", command_mode.command()),
        Style::default(),
    );

    surface
        .get_mut(
            area.x + u16::try_from(command_mode.cursor()).unwrap() + 1,
            area.y,
        )
        .set_bg(YELLOW);
}
