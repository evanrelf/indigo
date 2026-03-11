use illuso_term::*;
use illuso_tui::Terminal;
use std::{
    io::{self, Write as _},
    mem,
};

fn main() -> io::Result<()> {
    let mut terminal = Terminal::init()?;
    let (_, width) = terminal.size()?;

    let mut user_message = String::new();
    let mut width = width;
    let mut rng_state: u32 = 12345;

    draw_line(&mut terminal, width)?;
    redraw_input_nosync(&mut terminal, &user_message, width)?;
    terminal.flush()?;

    loop {
        match terminal.read_event()? {
            Event::KeyPress(key) | Event::KeyRepeat(key) => match key.code {
                KeyCode::Char(char @ ' '..='~') if key.modifiers.is_empty() => {
                    user_message.push(char);
                    redraw_input(&mut terminal, &user_message, width)?;
                }
                KeyCode::Backspace if key.modifiers.is_empty() => {
                    user_message.pop();
                    redraw_input(&mut terminal, &user_message, width)?;
                }
                KeyCode::Enter if key.modifiers.is_empty() && !user_message.is_empty() => {
                    let msg = mem::take(&mut user_message);

                    write!(terminal, "{}", SYNC_UPDATE_SET)?;
                    terminal.flush()?;

                    // Move up to the ─ line row, clear it and everything below
                    write!(terminal, "{}\r{CLEAR_TO_END_OF_SCREEN}", CursorUp(1))?;

                    // Print user message
                    write!(
                        terminal,
                        "{BOLD}user:{RESET} {}{msg}{RESET}\r\n\n",
                        Fg(Color::Cyan),
                    )?;

                    // Print assistant response
                    rng_state = xorshift(rng_state);
                    let response =
                        ASSISTANT_MESSAGES[rng_state as usize % ASSISTANT_MESSAGES.len()];
                    write!(
                        terminal,
                        "{BOLD}assistant:{RESET} {}{response}{RESET}\r\n\n",
                        Fg(Color::Green),
                    )?;

                    // Redraw the line and input placeholder
                    draw_line(&mut terminal, width)?;
                    redraw_input_nosync(&mut terminal, &user_message, width)?;

                    write!(terminal, "{}", SYNC_UPDATE_RESET)?;
                    terminal.flush()?;
                }
                KeyCode::Char('c' | 'd') if key.modifiers == KeyModifiers::CTRL => {
                    // Clear the ─ line and input before exiting
                    write!(terminal, "{}\r{CLEAR_TO_END_OF_SCREEN}", CursorUp(1))?;
                    terminal.flush()?;
                    break;
                }
                _ => {}
            },
            Event::Resize {
                width: new_width, ..
            } => {
                let up = widget_up(width, user_message.len(), new_width);
                width = new_width;
                write!(terminal, "{}", SYNC_UPDATE_SET)?;
                terminal.flush()?;
                // Move up past all wrapped remnants, clear, redraw
                write!(terminal, "{}\r{CLEAR_TO_END_OF_SCREEN}", CursorUp(up))?;
                draw_line(&mut terminal, width)?;
                redraw_input_nosync(&mut terminal, &user_message, width)?;
                write!(terminal, "{}", SYNC_UPDATE_RESET)?;
                terminal.flush()?;
            }
            _ => {}
        }
    }

    Ok(())
}

/// Calculate how many rows to move up from the input line to reach the top of
/// the widget (the ─ line). After a resize, the old content may have reflowed
/// and wrapped across more rows than before.
fn widget_up(old_width: u16, input_len: usize, current_width: u16) -> u16 {
    let w = current_width.max(1) as usize;
    let line_rows = (old_width.max(1) as usize).div_ceil(w);
    let input_display = input_len.min(old_width as usize);
    let input_rows = if input_display == 0 {
        1
    } else {
        input_display.div_ceil(w)
    };
    #[allow(clippy::cast_possible_truncation)]
    let up = (line_rows + input_rows - 1) as u16;
    up
}

fn draw_line(terminal: &mut Terminal, width: u16) -> io::Result<()> {
    // Move to column 1, draw horizontal line, then newline to input area
    write!(terminal, "\r")?;
    for _ in 0..width {
        write!(terminal, "─")?;
    }
    write!(terminal, "\r\n")?;
    Ok(())
}

fn redraw_input(terminal: &mut Terminal, input: &str, width: u16) -> io::Result<()> {
    write!(terminal, "{}", SYNC_UPDATE_SET)?;
    terminal.flush()?;
    redraw_input_nosync(terminal, input, width)?;
    write!(terminal, "{}", SYNC_UPDATE_RESET)?;
    terminal.flush()?;
    Ok(())
}

fn redraw_input_nosync(terminal: &mut Terminal, input: &str, width: u16) -> io::Result<()> {
    // Move to start of input line, clear it, write truncated input
    write!(terminal, "\r{CLEAR_LINE}")?;
    let max_chars = width.saturating_sub(2) as usize;
    if input.is_empty() {
        let placeholder = "type something";
        let truncated = &placeholder[..placeholder.len().min(max_chars)];
        write!(terminal, "{}{truncated}{RESET}{}", Fg(Color::White), CursorColumn(1))?;
    } else if input.len() > max_chars {
        // overflow: hidden — show only the last `max_chars` characters
        let start = input.len() - max_chars;
        write!(terminal, "{}", &input[start..])?;
    } else {
        write!(terminal, "{input}")?;
    }
    Ok(())
}

fn xorshift(mut x: u32) -> u32 {
    assert!(x != 0);
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    x
}

const ASSISTANT_MESSAGES: &[&str] = &[
    "That's craaaazy dude",
    "Wait, say that again?",
    "I'm happy for you. Or sorry that happened.",
    "I see",
    "k",
    "heyyy",
    "You're absolutely right!",
];
