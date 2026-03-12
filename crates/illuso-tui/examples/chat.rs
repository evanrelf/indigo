use illuso_term::{
    BOLD, CLEAR_LINE, CLEAR_TO_END_OF_SCREEN, Color, CursorUp, Event, Fg, KeyCode, KeyModifiers,
    RESET, SYNC_UPDATE_RESET, SYNC_UPDATE_SET,
};
use illuso_tui::Terminal;
use std::{
    cmp::{max, min},
    io::{self, Write as _},
    mem,
};

fn main() -> io::Result<()> {
    let mut terminal = Terminal::init()?;
    let (_, width) = terminal.size()?;

    let mut state = State {
        width,
        ..State::default()
    };

    while !state.quit {
        let event = terminal.read_event()?;
        update(&mut state, event);
        render(&state, &mut terminal)?;
    }

    Ok(())
}

#[derive(Default)]
struct State {
    quit: bool,
    prev_width: Option<u16>,
    width: u16,
    queued_message: Option<String>,
    message: String,
}

fn update(state: &mut State, event: Event) {
    state.prev_width = None;
    state.queued_message = None;

    match event {
        Event::KeyPress(key) | Event::KeyRepeat(key) => match key.code {
            KeyCode::Char(char @ ' '..='~') if key.modifiers.is_empty() => {
                state.message.push(char);
            }
            KeyCode::Backspace if key.modifiers.is_empty() => {
                state.message.pop();
            }
            KeyCode::Enter if key.modifiers.is_empty() && !state.message.is_empty() => {
                let message = mem::take(&mut state.message);
                state.queued_message = Some(message);
            }
            KeyCode::Char('u') if key.modifiers == KeyModifiers::CTRL => state.message.clear(),
            KeyCode::Char('c' | 'd') if key.modifiers == KeyModifiers::CTRL => {
                state.quit = true;
            }
            _ => {}
        },
        Event::Resize { width, .. } => {
            state.prev_width = Some(state.width);
            state.width = width;
        }
        _ => {}
    }
}

fn render(state: &State, terminal: &mut Terminal) -> io::Result<()> {
    if state.quit {
        render_quit(state, terminal)?;
        return Ok(());
    }

    write!(terminal, "{}", SYNC_UPDATE_SET)?;
    terminal.flush()?;

    render_clear(state, terminal)?;

    if let Some(message) = &state.queued_message {
        render_user_message(terminal, message)?;
        render_assistant_message(state, terminal)?;
    }

    render_line(state, terminal)?;
    render_input(state, terminal)?;

    write!(terminal, "{}", SYNC_UPDATE_RESET)?;
    terminal.flush()?;

    Ok(())
}

fn render_clear(state: &State, terminal: &mut Terminal) -> io::Result<()> {
    let height = match state.prev_width {
        Some(prev_width) => {
            let width = max(1, state.width) as usize;
            let line_height = (max(1, prev_width) as usize).div_ceil(width);
            let input_display = min(state.message.len(), prev_width as usize);
            let input_height = if input_display == 0 {
                1
            } else {
                input_display.div_ceil(width)
            };
            #[allow(clippy::cast_possible_truncation)]
            {
                (line_height + input_height - 1) as u16
            }
        }
        None => 1,
    };

    write!(terminal, "{}\r{}", CursorUp(height), CLEAR_TO_END_OF_SCREEN)?;

    Ok(())
}

fn render_user_message(terminal: &mut Terminal, message: &str) -> io::Result<()> {
    write!(
        terminal,
        "{BOLD}{}user:{RESET} {message}\r\n\n",
        Fg(Color::Blue)
    )?;
    Ok(())
}

fn render_assistant_message(state: &State, terminal: &mut Terminal) -> io::Result<()> {
    let message = assistant_message(state);
    write!(
        terminal,
        "{BOLD}{}assistant:{RESET} {message}\r\n\n",
        Fg(Color::Red)
    )?;
    Ok(())
}

fn render_line(state: &State, terminal: &mut Terminal) -> io::Result<()> {
    write!(terminal, "\r")?;
    for _ in 0..state.width {
        write!(terminal, "─")?;
    }
    write!(terminal, "\r\n")?;
    Ok(())
}

fn render_input(state: &State, terminal: &mut Terminal) -> io::Result<()> {
    write!(terminal, "\r{}", CLEAR_LINE)?;

    let max_chars = state.width as usize;

    if state.message.is_empty() {
        let placeholder = "say something";
        let end = min(placeholder.len(), max_chars);
        write!(
            terminal,
            "{}{}{}\r",
            Fg(Color::White),
            &placeholder[..end],
            RESET,
        )?;
    } else {
        let start = state.message.len().saturating_sub(max_chars - 1);
        write!(terminal, "{}", &state.message[start..])?;
    }

    Ok(())
}

fn render_quit(state: &State, terminal: &mut Terminal) -> io::Result<()> {
    write!(terminal, "{}", SYNC_UPDATE_SET)?;
    terminal.flush()?;

    render_clear(state, terminal)?;

    write!(terminal, "{}", SYNC_UPDATE_RESET)?;
    terminal.flush()?;

    Ok(())
}

fn assistant_message(state: &State) -> &'static str {
    let message = state.queued_message.as_deref().unwrap_or("");

    // Arbitrary pieces of state
    let width = u32::from(state.width);
    let first_char = message.chars().next().map_or(1, |char| u32::from(char));
    let length = message.len();

    // Combined in a dumbass way
    let seed = width
        .wrapping_mul(31)
        .wrapping_add(first_char)
        .wrapping_mul(31)
        .wrapping_add(u32::try_from(length).unwrap())
        | 1;

    // To get a pseudorandom number
    let random = xorshift(seed);

    // To pick an assistant message
    ASSISTANT_MESSAGES[random as usize % ASSISTANT_MESSAGES.len()]

    // And avoid depending on `fastrand` just for an example program
}

fn xorshift(mut x: u32) -> u32 {
    assert!(x != 0);
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    x
}

const ASSISTANT_MESSAGES: &[&str] = &[
    "That's crazy dude",
    "Wait, say that again?",
    "I'm happy for you. Or sorry that happened.",
    "I see",
    "k",
    "heyyy",
    "You're absolutely right!",
    "Yes",
    "No",
    "Maybe",
    "Yeah nah",
    "Nah yeah",
    "And how does that make you feel?",
    "frfr no cap on god",
];
