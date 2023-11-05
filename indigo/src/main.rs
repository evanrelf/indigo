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

use crate::macros::key_matches;
use anyhow::Context as _;
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{Event, EventStream, MouseEventKind};
use indigo_core::{Buffer, Editor, Mode};
use tokio_stream::StreamExt as _;
use tracing::Level;

#[derive(clap::Parser)]
struct Args {
    /// Files to open
    files: Vec<Utf8PathBuf>,

    /// Write logs to file for debugging
    #[arg(long)]
    log_file: Option<Utf8PathBuf>,
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

    let mut buffers = Vec::with_capacity(args.files.len());

    for file in args.files {
        buffers.push(Buffer::open(file).context("Failed to open buffer")?);
    }

    let current_buffer = if buffers.is_empty() { None } else { Some(0) };

    let editor = Editor::new(buffers, current_buffer, Mode::default());

    run(editor).await
}

async fn run(mut editor: Editor) -> anyhow::Result<()> {
    let mut terminal = terminal::enter().context("Failed to enter terminal")?;

    let mut event_stream = EventStream::new();

    loop {
        terminal
            .draw(|frame| ui::render(&editor, frame.size(), frame.buffer_mut()))
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

enum ControlFlow {
    Continue,
    Quit,
}

macro_rules! quit {
    () => {{
        return Ok(ControlFlow::Quit);
    }};
}

pub(crate) use quit;

fn update(editor: &mut Editor, event: &Event) -> anyhow::Result<ControlFlow> {
    match event {
        Event::Mouse(mouse) => match mouse.kind {
            MouseEventKind::ScrollUp => {
                if let Some(buffer) = editor.current_buffer_mut() {
                    buffer.scroll_up_mut(3);
                }
            }
            MouseEventKind::ScrollDown => {
                if let Some(buffer) = editor.current_buffer_mut() {
                    buffer.scroll_down_mut(3);
                }
            }
            _ => {}
        },
        Event::Key(key) if key_matches!(key, CONTROL 'p') => {
            panic!()
        }
        Event::Key(key) if key_matches!(key, CONTROL 'c') => {
            anyhow::bail!("Ctrl-C");
        }
        Event::Key(key) if key_matches!(key, 'q') => {
            quit!();
        }
        _ => {}
    }

    Ok(ControlFlow::Continue)
}
