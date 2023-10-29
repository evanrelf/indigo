mod conversion;
mod direction;
mod macros;
mod position;
mod range;
mod rope;
mod selection_range;
mod terminal;

use clap::Parser as _;
use crossterm::{event, style, ExecutableCommand as _};
use std::path::PathBuf;
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

    stdout.execute(style::Print("Hello, world!")).unwrap();

    let mut event_stream = event::EventStream::new();

    loop {
        use crate::macros::key_matches;
        use crossterm::event::Event::*;

        let event = match event_stream.next().await {
            Some(Ok(event)) => event,
            Some(Err(error)) => panic!("Error: {error}"),
            None => break,
        };

        tracing::debug!("event: {event:?}");

        match event {
            Key(key) if key_matches!(key, CONTROL 'p') => panic!(),
            Key(key) if key_matches!(key, CONTROL 'c') => break,
            Key(key) if key_matches!(key, 'q') => break,
            _ => continue,
        }
    }

    Ok(())
}
