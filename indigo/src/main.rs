#![warn(clippy::pedantic)]
#![allow(
    clippy::bool_to_int_with_if,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]

mod macros;
mod terminal;
mod tui;
mod ui;

use crate::{
    tui::{ControlFlow, Tui},
    ui::Indigo,
};
use anyhow::Context as _;
use clap::Parser as _;
use crossterm::event::EventStream;
use indigo_core::{Buffer, Editor, Mode};
use std::path::PathBuf;
use tokio_stream::StreamExt as _;
use tracing::Level;

#[derive(clap::Parser)]
struct Args {
    /// Files to open
    files: Vec<PathBuf>,

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

    let mut terminal = crate::terminal::enter().context("Failed to enter terminal")?;

    let mut event_stream = EventStream::new();

    let mut buffers = Vec::with_capacity(args.files.len());

    for file in args.files {
        buffers.push(Buffer::open(file).context("Failed to open buffer")?);
    }

    let editor = Editor {
        buffers,
        mode: Mode::default(),
    };

    let mut tui = Tui { editor };

    loop {
        terminal
            .draw(|frame| frame.render_widget(Indigo, frame.size()))
            .context("Failed to draw to terminal")?;

        let event = event_stream
            .next()
            .await
            .context("No more crossterm events")?
            .context("Failed to get next crossterm event")?;

        tracing::debug!(?event);

        match tui.update(event).context("Failed to update state")? {
            ControlFlow::Continue => {}
            ControlFlow::Quit => break,
        }
    }

    Ok(())
}
