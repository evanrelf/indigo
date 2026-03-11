use illuso_term::*;
use illuso_tui::Terminal;
use std::{
    io::{self, Write as _},
    mem,
};

fn main() -> io::Result<()> {
    let mut terminal = Terminal::init()?;

    let mut user_message = String::new();

    // TODO: Query terminal size. Or should I read the resize event that's emitted when in-band
    // resize is enabled? Or should `Terminal` track the size for me?

    // TODO: Draw message box

    loop {
        match terminal.read_event()? {
            Event::KeyPress(key) => match key.code {
                KeyCode::Char(char @ ' '..='~') if key.modifiers.is_empty() => {
                    // TODO: Draw characters as they're typed
                    user_message.push(char.to_ascii_lowercase());
                }
                // TODO: Handle keys like `!` coming in as `Shift+1` and being treated as `1`
                KeyCode::Char(char @ ' '..='~') if key.modifiers == KeyModifiers::SHIFT => {
                    // TODO: Draw characters as they're typed
                    user_message.push(char.to_ascii_uppercase());
                }
                KeyCode::Enter if key.modifiers.is_empty() => {
                    let user_message = mem::take(&mut user_message);
                    // TODO: Draw user and assistant role text in bold
                    // TODO: Draw user and assistant messages in different colors
                    write!(terminal, "user: {user_message}\r\n")?;
                    // TODO: Print psuedorandom assistant message
                    write!(terminal, "assistant: You're absolutely right!\r\n")?;
                    terminal.flush()?;
                }
                KeyCode::Char('c' | 'd') if key.modifiers == KeyModifiers::CTRL => break,
                _ => {}
            },
            // TODO: Redraw message box on resize
            Event::Resize { height, width } => {}
            _ => {}
        }
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
