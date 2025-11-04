use crate::{
    editor::Editor,
    mode::{
        CommandMode, InsertMode, Mode, NormalMode, SeekDirection, SeekInclude, SeekMode, SeekSelect,
    },
};
use clap::Parser as _;
use std::{borrow::Cow, iter, num::NonZeroUsize, process::ExitCode};

pub fn set_count(editor: &mut Editor, count: Option<NonZeroUsize>) {
    editor.mode.set_count(count);
}

pub fn enter_normal_mode(editor: &mut Editor) {
    editor.mode = Mode::Normal(NormalMode::default());
}

pub fn enter_seek_mode(
    editor: &mut Editor,
    select: SeekSelect,
    include: SeekInclude,
    direction: SeekDirection,
) {
    editor.mode = Mode::Seek(SeekMode {
        select,
        include,
        direction,
    });
}

pub fn enter_insert_mode(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.reduce();
    editor.mode = Mode::Insert(InsertMode::default());
}

pub fn enter_command_mode(editor: &mut Editor) {
    editor.mode = Mode::Command(CommandMode::default());
}

pub fn move_left(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.move_left(count);
    editor.mode.set_count(None);
}

pub fn move_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.move_right(count);
    editor.mode.set_count(None);
}

pub fn move_up(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.move_up(count);
    editor.mode.set_count(None);
}

pub fn move_down(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.move_down(count);
    editor.mode.set_count(None);
}

pub fn seek(editor: &mut Editor, byte: u8) {
    use crate::mode::{
        SeekDirection::{Next, Prev},
        SeekInclude::{Onto, Until},
        SeekSelect::{Extend, Move},
    };
    let Mode::Seek(seek_mode) = &editor.mode else {
        panic!("Not in seek mode")
    };
    let mut range = editor.buffer.range_mut();
    match (&seek_mode.select, &seek_mode.include, &seek_mode.direction) {
        (Move, Until, Prev) => range.move_until_prev_byte(byte),
        (Extend, Until, Prev) => range.extend_until_prev_byte(byte),
        (Move, Until, Next) => range.move_until_next_byte(byte),
        (Extend, Until, Next) => range.extend_until_next_byte(byte),
        (Move, Onto, Prev) => range.move_onto_prev_byte(byte),
        (Extend, Onto, Prev) => range.extend_onto_prev_byte(byte),
        (Move, Onto, Next) => range.move_onto_next_byte(byte),
        (Extend, Onto, Next) => range.extend_onto_next_byte(byte),
    }
    editor.mode.set_count(None);
}

pub fn extend_left(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.extend_left(count);
    editor.mode.set_count(None);
}

pub fn extend_right(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.extend_right(count);
    editor.mode.set_count(None);
}

pub fn extend_up(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.extend_up(count);
    editor.mode.set_count(None);
}

pub fn extend_down(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    let mut range = editor.buffer.range_mut();
    range.extend_down(count);
    editor.mode.set_count(None);
}

pub fn flip(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.flip();
    editor.mode.set_count(None);
}

pub fn flip_forward(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.flip_forward();
    editor.mode.set_count(None);
}

pub fn reduce(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.reduce();
    editor.mode.set_count(None);
}

pub fn scroll_up(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll().saturating_sub(3);
    editor.buffer.scroll_to(line);
}

pub fn scroll_down(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll() + 3;
    editor.buffer.scroll_to(line);
}

pub fn scroll_half_page_up(editor: &mut Editor) {
    let line = editor
        .buffer
        .vertical_scroll()
        .saturating_sub(editor.terminal_height / 2);
    editor.buffer.scroll_to(line);
}

pub fn scroll_half_page_down(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll() + editor.terminal_height / 2;
    editor.buffer.scroll_to(line);
}

pub fn scroll_full_page_up(editor: &mut Editor) {
    let line = editor
        .buffer
        .vertical_scroll()
        .saturating_sub(editor.terminal_height);
    editor.buffer.scroll_to(line);
}

pub fn scroll_full_page_down(editor: &mut Editor) {
    let line = editor.buffer.vertical_scroll() + editor.terminal_height;
    editor.buffer.scroll_to(line);
}

pub fn insert_char(editor: &mut Editor, char: char) {
    if let Mode::Command(ref mut command_mode) = editor.mode {
        command_mode.cursor_mut().insert_char(char);
    } else {
        let mut range = editor.buffer.range_mut();
        range.insert_char(char);
        editor.mode.set_count(None);
    }
}

pub fn insert(editor: &mut Editor, text: &str) {
    if let Mode::Command(ref mut command_mode) = editor.mode {
        command_mode.cursor_mut().insert(text);
    } else {
        let mut range = editor.buffer.range_mut();
        range.insert(text);
        editor.mode.set_count(None);
    }
}

pub fn delete_before(editor: &mut Editor) {
    if let Mode::Command(ref mut command_mode) = editor.mode {
        if command_mode.cursor().char_offset() == 0 {
            enter_normal_mode(editor);
        } else {
            command_mode.cursor_mut().delete_before();
        }
    } else {
        let mut range = editor.buffer.range_mut();
        range.delete_before();
        editor.mode.set_count(None);
    }
}

pub fn delete(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.delete();
    editor.mode.set_count(None);
}

pub fn delete_after(editor: &mut Editor) {
    let mut range = editor.buffer.range_mut();
    range.delete_after();
    editor.mode.set_count(None);
}

pub fn undo(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    for _ in 1..=count {
        if !editor.buffer.undo().unwrap() {
            break;
        }
    }
    editor.mode.set_count(None);
}

pub fn redo(editor: &mut Editor) {
    let count = editor.mode.count().unwrap_or(NonZeroUsize::MIN).get();
    for _ in 1..=count {
        if !editor.buffer.redo().unwrap() {
            break;
        }
    }
    editor.mode.set_count(None);
}

// TODO: Move command handling into command mode code. This function should be very short.
pub fn run_command(editor: &mut Editor) -> anyhow::Result<()> {
    #[derive(clap::Parser)]
    #[clap(
        disable_help_flag = true,
        disable_help_subcommand = true,
        override_usage = ""
    )]
    enum Command {
        Echo {
            #[clap(long)]
            error: bool,
            message: Vec<String>,
        },
        #[clap(alias = "w")]
        Write,
        #[clap(alias = "q")]
        Quit { exit_code: Option<u8> },
        #[clap(name = "quit!", alias = "q!")]
        QuitForce { exit_code: Option<u8> },
        #[clap(alias = "wq")]
        WriteQuit { exit_code: Option<u8> },
    }

    let Mode::Command(command_mode) = &editor.mode else {
        return Ok(());
    };

    let command = Cow::<str>::from(command_mode.text().rope());

    let Ok(args) = shellwords::split(&command) else {
        editor.message = Some(Err(String::from("Invalid command")));
        editor.mode = Mode::Normal(NormalMode::default());
        return Ok(());
    };

    if args.is_empty() {
        editor.mode = Mode::Normal(NormalMode::default());
        return Ok(());
    }

    let args = iter::once(String::from("indigo")).chain(args);

    let command = match Command::try_parse_from(args) {
        Ok(command) => command,
        Err(error) => {
            let error = error.to_string();
            let error = error
                .strip_prefix("error: ")
                .unwrap_or(&error)
                .lines()
                .next()
                .unwrap_or("");
            editor.message = Some(Err(error.to_string()));
            editor.mode = Mode::Normal(NormalMode::default());
            return Ok(());
        }
    };

    match command {
        Command::Echo { error, message } => {
            if error {
                editor.message = Some(Err(message.join(" ")));
            } else {
                editor.message = Some(Ok(message.join(" ")));
            }
        }
        Command::Write => {
            // TODO: Don't panic if save fails.
            editor.buffer.save(&mut editor.io)?;
        }
        Command::Quit { exit_code } => {
            if editor.buffer.modified {
                editor.message = Some(Err(String::from("Unsaved changes")));
            } else {
                editor.exit = if let Some(exit_code) = exit_code {
                    Some(ExitCode::from(exit_code))
                } else {
                    Some(ExitCode::SUCCESS)
                };
            }
        }
        Command::QuitForce { exit_code } => {
            editor.exit = if let Some(exit_code) = exit_code {
                Some(ExitCode::from(exit_code))
            } else {
                Some(ExitCode::SUCCESS)
            };
        }
        Command::WriteQuit { exit_code } => {
            // TODO: Don't panic if save fails.
            editor.buffer.save(&mut editor.io)?;
            editor.exit = if let Some(exit_code) = exit_code {
                Some(ExitCode::from(exit_code))
            } else {
                Some(ExitCode::SUCCESS)
            };
        }
    }

    editor.mode = Mode::Normal(NormalMode::default());

    Ok(())
}

pub fn exit(editor: &mut Editor, exit_code: u8) {
    editor.exit = Some(ExitCode::from(exit_code));
}
