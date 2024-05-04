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

    let mut areas = Areas::default();

    loop {
        terminal.draw(|frame| {
            areas = Areas::new(&editor, frame.size());
            let surface = frame.buffer_mut();
            render(&editor, areas, surface);
        })?;

        let quit = handle_event(&mut editor, areas, &event::read()?);

        if quit {
            break;
        }
    }

    Ok(())
}

#[derive(Clone, Copy, Default)]
struct Areas {
    text: Rect,
}

impl Areas {
    fn new(_editor: &Editor, area: Rect) -> Self {
        Self { text: area }
    }
}

fn render(editor: &Editor, areas: Areas, surface: &mut Surface) {
    render_text(editor, areas.text, surface);
}

fn render_text(editor: &Editor, area: Rect, surface: &mut Surface) {
    let vertical_scroll = 0;
    let horizontal_scroll = 0;

    'line: for (y, line) in editor.text.lines_at(vertical_scroll).enumerate() {
        let y = area.top() + u16::try_from(y).unwrap();

        if y >= area.bottom() {
            break 'line;
        }

        let Some(line) = line.get_slice(horizontal_scroll..) else {
            continue 'line;
        };

        let mut x = area.x;

        'grapheme: for grapheme in line.graphemes() {
            if x >= area.width {
                continue 'line;
            }

            if let Some('\t') = grapheme.get_char(0) {
                surface.set_stringn(x, y, "        ", 8, Style::default());
                x += 8;
                continue 'grapheme;
            };

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

fn handle_event(editor: &mut Editor, _areas: Areas, event: &Event) -> bool {
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
