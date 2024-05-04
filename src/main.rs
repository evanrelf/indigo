mod terminal;

use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{self, Event, KeyCode, KeyModifiers};
use indigo::{actions, Editor, RopeExt as _};
use ratatui::prelude::{Buffer as Surface, Rect, Style};
use std::borrow::Cow;

#[derive(Debug, clap::Parser)]
struct Args {
    file: Utf8PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut editor = Editor::new(args.file)?;

    let mut terminal = terminal::enter()?;

    let mut area = Rect::default();

    loop {
        terminal.draw(|frame| {
            area = frame.size();
            let surface = frame.buffer_mut();
            render(&editor, area, surface);
        })?;

        let quit = handle_event(&mut editor, area, &event::read()?);

        if quit {
            break;
        }
    }

    Ok(())
}

fn render(editor: &Editor, area: Rect, surface: &mut Surface) {
    render_text(editor, area, surface);
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    let vertical_scroll = 0;
    let horizontal_scroll = 0;

    'line: for y in area.top()..area.bottom() {
        let line_index = vertical_scroll + usize::from(y - area.top());

        let Some(line) = editor.text.get_line(line_index) else {
            break 'line;
        };

        let Some(line) = line.get_slice(horizontal_scroll..) else {
            continue 'line;
        };

        let mut x = area.x;

        'grapheme: for grapheme in line.graphemes() {
            if x >= area.width {
                continue 'line;
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

fn handle_event(editor: &mut Editor, _area: Rect, event: &Event) -> bool {
    let mut quit = false;

    #[allow(clippy::single_match)]
    match event {
        Event::Key(key_event) => match (key_event.modifiers, key_event.code) {
            (KeyModifiers::NONE, KeyCode::Char('h')) => actions::move_left(editor),
            (KeyModifiers::NONE, KeyCode::Char('l')) => actions::move_right(editor),
            (KeyModifiers::CONTROL, KeyCode::Char('c')) => quit = true,
            _ => {}
        },
        _ => {}
    }

    quit
}
