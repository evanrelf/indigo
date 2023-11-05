#![warn(clippy::pedantic, clippy::use_self)]
#![allow(
    clippy::bool_to_int_with_if,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]

mod macros;
mod terminal;
mod ui;

use crate::{macros::key_matches, ui::Indigo};
use anyhow::Context as _;
use clap::Parser as _;
use crossterm::event::{Event, EventStream};
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

    let mut editor = Editor {
        buffers,
        mode: Mode::default(),
    };

    loop {
        terminal
            .draw(|frame| frame.render_widget(Indigo::new(&editor), frame.size()))
            .context("Failed to draw to terminal")?;

        let event = event_stream
            .next()
            .await
            .context("No more crossterm events")?
            .context("Failed to get next crossterm event")?;

        tracing::debug!(?event);

        match update(&mut editor, &event).context("Failed to update state")? {
            ControlFlow::Continue => {}
            ControlFlow::Quit => break,
        }
    }

    Ok(())
}

pub enum ControlFlow {
    Continue,
    Quit,
}

pub fn update(_editor: &mut Editor, event: &Event) -> anyhow::Result<ControlFlow> {
    match event {
        Event::Key(key) if key_matches!(key, CONTROL 'p') => {
            panic!()
        }
        Event::Key(key) if key_matches!(key, CONTROL 'c') => {
            anyhow::bail!("Ctrl-C");
        }
        Event::Key(key) if key_matches!(key, 'q') => {
            return Ok(ControlFlow::Quit);
        }
        _ => {}
    }

    Ok(ControlFlow::Continue)
}