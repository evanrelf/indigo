#![warn(clippy::pedantic)]
#![allow(
    clippy::bool_to_int_with_if,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]

mod macros;
mod terminal;
mod tui;

use crate::tui::{ControlFlow, Tui};
use anyhow::Context as _;
use clap::Parser as _;
use crossterm::event::EventStream;
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
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    if let Some(path) = args.log_file {
        let file = std::fs::File::create(path).context("Failed to create log file")?;
        tracing_subscriber::fmt()
            .with_max_level(Level::DEBUG)
            .with_writer(file)
            .init();
    }

    let _terminal = crate::terminal::enter();

    let mut event_stream = EventStream::new();

    let mut tui = Tui::default();

    loop {
        tui.view().context("Failed to view")?;
        let event = event_stream
            .next()
            .await
            .context("No more events")?
            .context("Failed to get next event")?;
        tracing::debug!(?event);
        match tui.update(event).context("Failed to update")? {
            ControlFlow::Continue => {}
            ControlFlow::Quit => break,
        }
    }

    Ok(())
}
