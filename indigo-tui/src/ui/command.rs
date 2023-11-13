use crate::ui::colors::{BG_GRAY, BG_YELLOW};
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

    // TODO: Don't use `Paragraph` widget here because it's buggy.
    //
    // When the command is only spaces, and you exceed a single line (triggering wrapping), drawing
    // gets offset by one. Also, it doesn't draw the background color properly all the time?
    Paragraph::new(format!(":{command}"))
        .style(Style::default().bg(BG_GRAY))
        .wrap(Wrap { trim: false })
        .render(area, surface);

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
