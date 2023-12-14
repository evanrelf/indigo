mod key;
mod terminal;
mod ui;

use crate::{
    key::macros::{k, key_modifiers},
    ui::areas::Areas,
};
use anyhow::Context as _;
use camino::Utf8PathBuf;
use clap::Parser as _;
use crossterm::event::{Event, EventStream, KeyCode, MouseEventKind};
use indigo_core::{
    command::{self, Quit},
    Command, CommandMode, Editor, File, InsertMode, Mode, NormalMode,
};
use smol::stream::StreamExt as _;
use std::process::ExitCode;

#[derive(clap::Parser)]
struct Args {
    /// Files to open
    files: Vec<Utf8PathBuf>,

    /// Write logs to file for debugging
    #[arg(long)]
    log_file: Option<Utf8PathBuf>,
}

fn main() -> anyhow::Result<ExitCode> {
    // TODO: This feels suboptimal
    let executor = Box::leak(Box::new(smol::LocalExecutor::new()));
    let task = executor.spawn(indigo_tui(executor));
    smol::block_on(executor.run(task))
}

async fn indigo_tui(executor: &smol::LocalExecutor<'_>) -> anyhow::Result<ExitCode> {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }

    let args = Args::parse();

    if let Some(path) = args.log_file {
        if std::env::var("RUST_LOG").is_err() {
            std::env::set_var("RUST_LOG", "trace");
        }

        let file = std::fs::File::create(path).context("Failed to create log file")?;

        tracing_subscriber::fmt().with_writer(file).init();
    }

    let editor = {
        let mut join_handles = args
            .files
            .iter()
            .map(|path| executor.spawn(File::open(path.clone())))
            // Need to use `collect` to force the iterator
            .collect::<Vec<_>>()
            .into_iter();

        if let Some(join_handle) = join_handles.next() {
            let file = join_handle.await.context("Failed to open file")?;
            let mut editor = Editor::new(file);

            for join_handle in join_handles {
                let file = join_handle.await.context("Failed to open file")?;
                editor.insert_file(file);
            }

            editor
        } else {
            Editor::new(File::new("scratch"))
        }
    };

    run(editor).await
}

async fn run(mut editor: Editor) -> anyhow::Result<ExitCode> {
    let mut terminal = terminal::enter().context("Failed to enter terminal")?;

    let mut event_stream = EventStream::new();

    let mut areas = Areas::empty();

    loop {
        #[cfg(debug_assertions)]
        editor.assert_valid();

        terminal
            .draw(|frame| {
                areas = Areas::new(&editor, frame.size());
                ui::render(&editor, areas, frame.buffer_mut());
            })
            .context("Failed to draw to terminal")?;

        let event = event_stream
            .next()
            .await
            .context("No more crossterm events")?
            .context("Failed to get next crossterm event")?;

        if matches!(event, Event::Mouse(_)) {
            tracing::trace!(?event);
        } else {
            tracing::debug!(?event);
        }

        match update(&mut editor, areas, &event).context("Failed to update state")? {
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

// TODO: Treat `CONTROL 'B'` like `CONTROL SHIFT 'b'`

fn update(editor: &mut Editor, areas: Areas, event: &Event) -> anyhow::Result<ControlFlow> {
    match editor.mode() {
        Mode::Normal(_) => update_normal(editor, areas, event),
        Mode::Insert(_) => update_insert(editor, areas, event),
        Mode::Command(_) => update_command(editor, areas, event),
    }
}

fn update_normal(editor: &mut Editor, areas: Areas, event: &Event) -> anyhow::Result<ControlFlow> {
    match event {
        // Scrolling
        Event::Mouse(mouse) => match mouse.kind {
            MouseEventKind::ScrollUp => {
                editor.current_window_mut().scroll_up(3);
            }
            MouseEventKind::ScrollDown => {
                editor.current_window_mut().scroll_down(3);
            }
            _ => {}
        },
        Event::Key(key) if k!(key, Up) => {
            editor.current_window_mut().scroll_up(10);
        }
        Event::Key(key) if k!(key, Down) => {
            editor.current_window_mut().scroll_down(10);
        }
        Event::Key(key) if k!(key, Left) => {
            editor.current_window_mut().scroll_left(10);
        }
        Event::Key(key) if k!(key, Right) => {
            editor.current_window_mut().scroll_right(10);
        }
        Event::Key(key) if k!(key, SHIFT, Up) => {
            editor.current_window_mut().scroll_up(1);
        }
        Event::Key(key) if k!(key, SHIFT, Down) => {
            editor.current_window_mut().scroll_down(1);
        }
        Event::Key(key) if k!(key, SHIFT, Left) => {
            editor.current_window_mut().scroll_left(1);
        }
        Event::Key(key) if k!(key, SHIFT, Right) => {
            editor.current_window_mut().scroll_right(1);
        }
        Event::Key(key) if k!(key, CONTROL, 'u') => {
            editor
                .current_window_mut()
                .scroll_up(usize::from(areas.buffer.height) / 2);
        }
        Event::Key(key) if k!(key, CONTROL, 'd') => {
            editor
                .current_window_mut()
                .scroll_down(usize::from(areas.buffer.height) / 2);
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

fn update_insert(editor: &mut Editor, _areas: Areas, event: &Event) -> anyhow::Result<ControlFlow> {
    match event {
        Event::Key(key)
            if matches!(key.code, KeyCode::Char(_))
                && (key.modifiers == key_modifiers!()
                    || key.modifiers == key_modifiers!(SHIFT)) =>
        {
            let KeyCode::Char(c) = key.code else {
                unreachable!();
            };
            editor
                .current_file_mut()
                .buffer_mut()
                .selection_mut()
                .insert_char(c);
        }
        Event::Key(key) if k!(key, Enter) => {
            editor
                .current_file_mut()
                .buffer_mut()
                .selection_mut()
                .insert_char('\n');
        }
        Event::Paste(s) => {
            editor
                .current_file_mut()
                .buffer_mut()
                .selection_mut()
                .insert(s);
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

// TODO: Support pasting large strings (use bracketed paste mode, don't `insert_char` a million
// times)
fn update_command(
    editor: &mut Editor,
    _areas: Areas,
    event: &Event,
) -> anyhow::Result<ControlFlow> {
    let Mode::Command(command_mode) = editor.mode_mut() else {
        unreachable!();
    };

    match event {
        Event::Key(key) if k!(key, Left) || k!(key, CONTROL, 'b') => {
            command_mode.move_backward(1);
        }
        Event::Key(key) if k!(key, Right) || k!(key, CONTROL, 'f') => {
            command_mode.move_forward(1);
        }
        Event::Key(key) if k!(key, Left) || k!(key, ALT, 'b') => {
            command_mode.move_backward_word();
        }
        Event::Key(key) if k!(key, Right) || k!(key, ALT, 'f') => {
            command_mode.move_forward_word();
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
        Event::Paste(s) => {
            command_mode.insert(s);
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
