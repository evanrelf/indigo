use crate::macros::key_matches;
use crossterm::{
    cursor::MoveTo,
    event::Event,
    style,
    terminal::{Clear, ClearType},
    QueueableCommand as _,
};
use std::io::Write as _;

#[derive(Default)]
pub struct Tui {
    pub mouse_position: (u16, u16),
}

impl Tui {
    pub fn update(&mut self, event: Event) -> anyhow::Result<bool> {
        match event {
            Event::Mouse(mouse_event) => {
                self.mouse_position = (mouse_event.row, mouse_event.column);
                Ok(false)
            }
            Event::Key(key) if key_matches!(key, CONTROL 'p') => {
                panic!()
            }
            Event::Key(key) if key_matches!(key, CONTROL 'c') => anyhow::bail!("Ctrl-C"),
            Event::Key(key) if key_matches!(key, 'q') => Ok(true),
            _ => Ok(false),
        }
    }

    pub fn view(&self) -> anyhow::Result<()> {
        std::io::stdout()
            .queue(MoveTo(0, 0))?
            .queue(Clear(ClearType::CurrentLine))?
            .queue(style::Print(format!("mouse: {:?}", self.mouse_position)))?
            .flush()?;

        Ok(())
    }
}
