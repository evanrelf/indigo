use ratatui::backend::CrosstermBackend;
use std::{
    io::Stdout,
    ops::{Deref, DerefMut},
};

pub struct Terminal(ratatui::Terminal<CrosstermBackend<Stdout>>);

impl Terminal {
    pub fn new() -> anyhow::Result<Self> {
        let stdout = std::io::stdout();
        let backend = CrosstermBackend::new(stdout);
        let terminal = ratatui::Terminal::new(backend)?;
        Ok(Self(terminal))
    }
}

impl Deref for Terminal {
    type Target = ratatui::Terminal<CrosstermBackend<Stdout>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Terminal {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub fn enter() -> anyhow::Result<()> {
    let mut stdout = std::io::stdout();

    crossterm::terminal::enable_raw_mode()?;

    crossterm::execute!(
        stdout,
        crossterm::terminal::EnterAlternateScreen,
        crossterm::cursor::Hide,
        crossterm::event::EnableMouseCapture,
    )?;

    Ok(())
}

pub fn exit() -> anyhow::Result<()> {
    let mut stdout = std::io::stdout();

    crossterm::execute!(
        stdout,
        crossterm::event::DisableMouseCapture,
        crossterm::cursor::Show,
        crossterm::terminal::LeaveAlternateScreen,
    )?;

    crossterm::terminal::disable_raw_mode()?;

    Ok(())
}
