use indigo_core::{Editor, Mode};
use ratatui::prelude::{Buffer as Surface, *};

pub fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    let Mode::Command(command_mode) = editor.mode() else {
        return;
    };

    let yellow = Color::Rgb(0xFF, 0xD3, 0x3D);
    // let light_yellow = Color::Rgb(0xFF, 0xF5, 0xB1);

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
        .set_bg(yellow);
}
