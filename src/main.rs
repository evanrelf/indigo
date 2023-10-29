mod conversion;
mod direction;
mod macros;
mod position;
mod range;
mod rope;
mod selection_range;
mod terminal;

use crate::macros::key_matches;
use clap::Parser as _;
use crossterm::{
    cursor::MoveTo,
    event::{Event, EventStream},
    style,
    terminal::{Clear, ClearType},
    QueueableCommand as _,
};
use std::{io::Write as _, path::PathBuf};
use tokio_stream::StreamExt as _;
use tracing::Level;

#[derive(clap::Parser)]
struct Args {
    /// Write logs to file for debugging
    #[arg(long)]
    log_file: Option<PathBuf>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    if let Some(path) = args.log_file {
        let file = std::fs::File::create(path)?;
        tracing_subscriber::fmt()
            .with_max_level(Level::DEBUG)
            .with_writer(file)
            .init();
    }

    let _terminal = terminal::enter();

    let mut stdout = std::io::stdout();

    let mut event_stream = EventStream::new();

    let mut mouse_position;

    loop {
        let event = match event_stream.next().await {
            Some(Ok(event)) => event,
            Some(Err(error)) => panic!("Error: {error}"),
            None => break,
        };

        tracing::debug!(?event);

        match event {
            Event::Mouse(mouse_event) => mouse_position = (mouse_event.row, mouse_event.column),
            Event::Key(key) if key_matches!(key, CONTROL 'p') => panic!(),
            Event::Key(key) if key_matches!(key, CONTROL 'c') => break,
            Event::Key(key) if key_matches!(key, 'q') => break,
            _ => continue,
        }

        stdout
            .queue(MoveTo(0, 0))?
            .queue(Clear(ClearType::CurrentLine))?
            .queue(style::Print(format!("mouse: {:?}", mouse_position)))?
            .flush()?;
    }

    Ok(())
}
