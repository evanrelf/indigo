pub fn enter() -> anyhow::Result<()> {
    let mut stdout = std::io::stdout();

    crossterm::terminal::enable_raw_mode()?;

    crossterm::execute!(
        stdout,
        crossterm::terminal::EnterAlternateScreen,
        crossterm::cursor::Hide,
    )?;

    Ok(())
}

pub fn exit() -> anyhow::Result<()> {
    let mut stdout = std::io::stdout();

    crossterm::execute!(
        stdout,
        crossterm::cursor::Show,
        crossterm::terminal::LeaveAlternateScreen,
    )?;

    crossterm::terminal::disable_raw_mode()?;

    Ok(())
}
