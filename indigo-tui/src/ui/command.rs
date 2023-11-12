use crate::ui::colors::YELLOW;
use indigo_core::{CommandMode, Editor, Mode};
use ratatui::{
    prelude::{Buffer as Surface, *},
    widgets::{Paragraph, Widget as _, Wrap},
};
use std::borrow::Cow;

// TODO: Paint over status elements a few characters ahead of the command, so it's clear the command
// text isn't part of the status

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let Mode::Command(command_mode) = editor.mode() else {
        return;
    };

    let command = Cow::<str>::from(command_mode.command());

    Paragraph::new(format!(":{command}"))
        .wrap(Wrap { trim: false })
        .render(area, surface);

    let cursor = u16::try_from(command_mode.cursor()).unwrap();

    let x = area.x + ((cursor + 1) % area.width);
    let y = area.y + ((cursor + 1) / area.width);

    surface.get_mut(x, y).set_bg(YELLOW);
}

pub fn lines(command_mode: &CommandMode, area: Rect) -> u16 {
    let chars = command_mode.command().len_chars() + 1;
    let lines = (chars / usize::from(area.width)) + 1;
    u16::try_from(lines).unwrap()
}
