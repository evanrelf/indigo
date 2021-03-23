use crossterm::{style::Print, ExecutableCommand, Result};
use std::io::stdout;

fn main() -> Result<()> {
    stdout().execute(Print("Hello, world!\n"))?;

    Ok(())
}
