use crate::{
    buffer::Buffer,
    cursor::{Cursor, CursorMut, CursorState},
    editor::Editor,
    event::{Event, KeyEvent},
    key::{KeyCode, is},
    mode::{
        Mode,
        normal::{NormalMode, enter_normal_mode},
    },
    text::Text,
};
use camino::Utf8PathBuf;
use clap::Parser as _;
use ropey::Rope;
use std::{borrow::Cow, iter, process::ExitCode};

#[derive(Default)]
pub struct CommandMode {
    text: Text,
    cursor: CursorState,
}

impl CommandMode {
    #[must_use]
    pub fn rope(&self) -> &Rope {
        self.text.rope()
    }

    pub fn cursor(&self) -> Cursor<'_> {
        let cursor = Cursor::new(&self.text, &self.cursor)
            .expect("Command mode text and cursor state are always kept valid");
        cursor.assert_invariants().unwrap();
        cursor
    }

    pub fn cursor_mut(&mut self) -> CursorMut<'_> {
        CursorMut::new(&mut self.text, &mut self.cursor)
            .expect("Command mode text and cursor state are always kept valid")
            .on_drop(|cursor| cursor.assert_invariants().unwrap())
    }
}

pub fn handle_event_command(editor: &mut Editor, event: &Event) -> anyhow::Result<bool> {
    let Mode::Command(_command_mode) = &editor.mode else {
        panic!("Not in command mode")
    };

    let mut handled = true;

    match event {
        Event::Key(KeyEvent { key, .. }) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, "<bs>") => delete_before(editor),
            _ if is(key, "<ret>") => exec_command(editor),
            (m, KeyCode::Char(c)) if m.is_empty() => insert_char(editor, char::from(c)),
            _ if is(key, "<c-c>") => editor.exit(1),
            _ => handled = false,
        },
    }

    Ok(handled)
}

pub fn enter_command_mode(editor: &mut Editor) {
    editor.mode = Mode::Command(CommandMode::default());
}

fn insert_char(editor: &mut Editor, char: char) {
    let Mode::Command(command_mode) = &mut editor.mode else {
        panic!("Not in command mode")
    };
    command_mode.cursor_mut().insert_char(char);
}

pub fn paste(editor: &mut Editor, text: &str) {
    let Mode::Command(command_mode) = &mut editor.mode else {
        panic!("Not in command mode")
    };
    command_mode.cursor_mut().insert(text);
}

fn delete_before(editor: &mut Editor) {
    let Mode::Command(command_mode) = &mut editor.mode else {
        panic!("Not in command mode")
    };
    if command_mode.cursor().is_at_start() {
        enter_normal_mode(editor);
    } else {
        command_mode.cursor_mut().delete_before();
    }
}

#[expect(clippy::too_many_lines)]
fn exec_command(editor: &mut Editor) {
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
        #[clap(alias = "e")]
        Edit { path: Utf8PathBuf },
        // TODO: Delete `edit!` once multiple buffers are supported.
        #[clap(name = "edit!", alias = "e!")]
        EditForce { path: Utf8PathBuf },
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
        return;
    };

    let command = Cow::<str>::from(command_mode.rope());

    let Ok(args) = shell_words::split(&command) else {
        editor.message = Some(Err(String::from("Invalid command")));
        editor.mode = Mode::Normal(NormalMode::default());
        return;
    };

    if args.is_empty() {
        editor.mode = Mode::Normal(NormalMode::default());
        return;
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
            return;
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
        Command::Edit { path } => {
            if editor.buffer.is_modified().unwrap_or(false) {
                editor.message = Some(Err(String::from("Unsaved changes")));
            } else if let Ok(buffer) = Buffer::open(&mut editor.fs, &path) {
                editor.buffer = buffer;
            } else {
                editor.message = Some(Err(format!("Failed to open {path}")));
            }
        }
        Command::EditForce { path } => {
            if let Ok(buffer) = Buffer::open(&mut editor.fs, &path) {
                editor.buffer = buffer;
            } else {
                editor.message = Some(Err(format!("Failed to open {path}")));
            }
        }
        Command::Write => {
            if editor.buffer.save(&mut editor.fs).is_err() {
                editor.message = Some(Err(String::from("Failed to save")));
            }
        }
        Command::Quit { exit_code } => {
            if editor.buffer.is_modified().unwrap_or(false) {
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
            if editor.buffer.save(&mut editor.fs).is_ok() {
                editor.exit = if let Some(exit_code) = exit_code {
                    Some(ExitCode::from(exit_code))
                } else {
                    Some(ExitCode::SUCCESS)
                };
            } else {
                editor.message = Some(Err(String::from("Failed to save")));
            }
        }
    }

    editor.mode = Mode::Normal(NormalMode::default());
}
