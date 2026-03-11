#![allow(unused)]

use illuso_term::*;
use illuso_tui::Terminal;
use std::{
    io::{self, Write as _},
    mem,
};

fn main() -> io::Result<()> {
    let mut terminal = Terminal::init()?;
    let (_, width) = terminal.size()?;

    let mut state = State { quit: false, width };

    loop {
        let event = terminal.read_event()?;

        update(&mut state, event);

        if state.quit {
            break;
        }

        render(&state, &mut terminal)?;
    }

    Ok(())
}

struct State {
    quit: bool,
    width: u16,
}

fn update(state: &mut State, event: Event) {
    match event {
        Event::KeyPress(key) | Event::KeyRepeat(key) => match key.code {
            KeyCode::Char('c' | 'd') if key.modifiers == KeyModifiers::CTRL => {
                state.quit = true;
            }
            _ => {}
        },
        _ => {}
    }
}

fn render(state: &State, terminal: &mut Terminal) -> io::Result<()> {
    write!(terminal, "{}{}Hello, world!", CLEAR_LINE, CursorColumn(1))?;
    terminal.flush()?;

    Ok(())
}
