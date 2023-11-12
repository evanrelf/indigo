#![warn(clippy::pedantic, clippy::use_self)]
#![allow(
    clippy::bool_to_int_with_if,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::module_name_repetitions
)]
// TODO: Remove
#![allow(dead_code)]

mod key;
mod terminal;
mod ui;

use crate::key::macros::{k, key_modifiers};
use anyhow::Context as _;
use camino::Utf8PathBuf;
use clap::Parser as _;
use clap_verbosity_flag::Verbosity;
use crossterm::event::{Event, EventStream, KeyCode, MouseEventKind};
use indigo_core::{
    command::{self, Quit},
    Buffer, Command, CommandMode, Editor, InsertMode, Mode, NormalMode,
};
use std::process::ExitCode;
use tokio_stream::StreamExt as _;
use tracing_log::AsTrace as _;

#[derive(clap::Parser)]
struct Args {
    /// Files to open
    files: Vec<Utf8PathBuf>,

    /// Write logs to file for debugging
    #[arg(long)]
    log_file: Option<Utf8PathBuf>,

    #[command(flatten)]
    verbosity: Verbosity,
}

#[tokio::main]
async fn main() -> anyhow::Result<ExitCode> {
    let args = Args::parse();

    if let Some(path) = args.log_file {
        let file = std::fs::File::create(path).context("Failed to create log file")?;
        tracing_subscriber::fmt()
            .with_max_level(args.verbosity.log_level_filter().as_trace())
            .with_writer(file)
            .init();
    }

    let editor = if args.files.is_empty() {
        Editor::default()
    } else {
        let mut buffers = Vec::with_capacity(args.files.len());
        for file in args.files {
            buffers.push(Buffer::open(file).context("Failed to open buffer")?);
        }
        Editor::new(buffers, 0, Mode::default())
    };

    run(editor).await
}

async fn run(mut editor: Editor) -> anyhow::Result<ExitCode> {
    let mut terminal = terminal::enter().context("Failed to enter terminal")?;

    let mut event_stream = EventStream::new();

    loop {
        #[cfg(debug_assertions)]
        editor.assert_valid();

        terminal
            .draw(|frame| ui::render(&editor, frame.size(), frame.buffer_mut()))
            .context("Failed to draw to terminal")?;

        let event = event_stream
            .next()
            .await
            .context("No more crossterm events")?
            .context("Failed to get next crossterm event")?;

        tracing::trace!(?event);

        match update(&mut editor, &event).context("Failed to update state")? {
            ControlFlow::Continue => {}
            ControlFlow::Quit(exit_code) => break Ok(exit_code),
        }
    }
}

enum ControlFlow {
    Continue,
    Quit(ExitCode),
}

macro_rules! quit {
    () => {{
        return Ok(ControlFlow::Quit(ExitCode::SUCCESS));
    }};
    ($x:expr) => {{
        return Ok(ControlFlow::Quit(ExitCode::from($x)));
    }};
}

pub(crate) use quit;

fn update(editor: &mut Editor, event: &Event) -> anyhow::Result<ControlFlow> {
    match editor.mode() {
        Mode::Normal(_) => update_normal(editor, event),
        Mode::Insert(_) => update_insert(editor, event),
        Mode::Command(_) => update_command(editor, event),
    }
}

fn update_normal(editor: &mut Editor, event: &Event) -> anyhow::Result<ControlFlow> {
    match event {
        // Scrolling
        Event::Mouse(mouse) => match mouse.kind {
            MouseEventKind::ScrollUp => {
                editor.current_buffer_mut().scroll_up(3);
            }
            MouseEventKind::ScrollDown => {
                editor.current_buffer_mut().scroll_down(3);
            }
            _ => {}
        },
        Event::Key(key) if k!(key, Up) => {
            editor.current_buffer_mut().scroll_up(1);
        }
        Event::Key(key) if k!(key, Down) => {
            editor.current_buffer_mut().scroll_down(1);
        }
        Event::Key(key) if k!(key, Left) => {
            editor.current_buffer_mut().scroll_left(1);
        }
        Event::Key(key) if k!(key, Right) => {
            editor.current_buffer_mut().scroll_right(1);
        }
        Event::Key(key) if k!(key, SHIFT, Up) => {
            editor.current_buffer_mut().scroll_up(10);
        }
        Event::Key(key) if k!(key, SHIFT, Down) => {
            editor.current_buffer_mut().scroll_down(10);
        }
        Event::Key(key) if k!(key, SHIFT, Left) => {
            editor.current_buffer_mut().scroll_left(10);
        }
        Event::Key(key) if k!(key, SHIFT, Right) => {
            editor.current_buffer_mut().scroll_right(10);
        }
        // Modes
        Event::Key(key) if k!(key, 'i') => {
            *editor.mode_mut() = Mode::Insert(InsertMode::default());
        }
        Event::Key(key) if k!(key, ':') => {
            *editor.mode_mut() = Mode::Command(CommandMode::default());
        }
        // Debug
        Event::Key(key) if k!(key, CONTROL, 'p') => {
            panic!();
        }
        Event::Key(key) if k!(key, CONTROL, 'c') => {
            anyhow::bail!("Ctrl-C");
        }
        _ => {}
    }

    Ok(ControlFlow::Continue)
}

fn update_insert(editor: &mut Editor, event: &Event) -> anyhow::Result<ControlFlow> {
    match event {
        // Modes
        Event::Key(key) if k!(key, Esc) => {
            *editor.mode_mut() = Mode::Normal(NormalMode::default());
        }
        // Debug
        Event::Key(key) if k!(key, CONTROL, 'p') => {
            panic!();
        }
        Event::Key(key) if k!(key, CONTROL, 'c') => {
            anyhow::bail!("Ctrl-C");
        }
        _ => {}
    }

    Ok(ControlFlow::Continue)
}

fn update_command(editor: &mut Editor, event: &Event) -> anyhow::Result<ControlFlow> {
    let Mode::Command(command_mode) = editor.mode_mut() else {
        unreachable!();
    };

    match event {
        Event::Key(key) if k!(key, Left) || k!(key, CONTROL, 'b') => {
            command_mode.move_left(1);
        }
        Event::Key(key) if k!(key, Right) || k!(key, CONTROL, 'f') => {
            command_mode.move_right(1);
        }
        Event::Key(key) if k!(key, CONTROL, 'a') => {
            command_mode.move_line_begin();
        }
        Event::Key(key) if k!(key, CONTROL, 'e') => {
            command_mode.move_line_end();
        }
        Event::Key(key) if k!(key, CONTROL, 'u') => {
            command_mode.clear_backward();
        }
        Event::Key(key) if k!(key, CONTROL, 'k') => {
            command_mode.clear_forward();
        }
        Event::Key(key)
            if matches!(key.code, KeyCode::Char(_))
                && (key.modifiers == key_modifiers!()
                    || key.modifiers == key_modifiers!(SHIFT)) =>
        {
            let KeyCode::Char(c) = key.code else {
                unreachable!();
            };
            command_mode.insert_char(c);
        }
        Event::Key(key) if k!(key, Backspace) => {
            if command_mode.command().len_chars() == 0 {
                *editor.mode_mut() = Mode::Normal(NormalMode::default());
            } else {
                command_mode.backspace();
            }
        }
        Event::Key(key) if k!(key, Enter) => {
            let command = command_mode.command().to_string();

            if !command.is_empty() {
                match command::parse(&command).context("Failed to parse command")? {
                    Command::Quit(Quit { exit_code }) => {
                        quit!(exit_code.unwrap_or(0));
                    }
                }
            }

            *editor.mode_mut() = Mode::Normal(NormalMode::default());
        }
        // Modes
        Event::Key(key) if k!(key, Esc) => {
            *editor.mode_mut() = Mode::Normal(NormalMode::default());
        }
        // Debug
        Event::Key(key) if k!(key, CONTROL, 'p') => {
            panic!();
        }
        Event::Key(key) if k!(key, CONTROL, 'c') => {
            anyhow::bail!("Ctrl-C");
        }
        _ => {}
    }

    Ok(ControlFlow::Continue)
}
