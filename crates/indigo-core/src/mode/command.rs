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
use ropey::Rope;
use std::{borrow::Cow, sync::Arc};

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

    // TODO: Add mappings for cursor movement.
    match event {
        Event::Key(KeyEvent { key, .. }) => match (key.modifiers, key.code) {
            _ if is(key, "<esc>") => enter_normal_mode(editor),
            _ if is(key, "<bs>") => delete_before(editor),
            _ if is(key, "<ret>") => exec_command(editor),
            (m, KeyCode::Char(c)) if m.is_empty() => insert_char(editor, char::from(c)),
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

fn exec_command(editor: &mut Editor) {
    let Mode::Command(command_mode) = &editor.mode else {
        return;
    };

    let command = match parse_command(command_mode.rope()) {
        Ok(command) => command,
        Err(error) => {
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
            if editor.window().buffer().is_modified().unwrap_or(false) {
                editor.message = Some(Err(String::from("Unsaved changes")));
            } else if let Ok(buffer) = Buffer::open(&editor.fs, &path) {
                *editor.window_mut().buffer_mut() = buffer;
            } else {
                editor.message = Some(Err(format!("Failed to open {path}")));
            }
        }
        Command::EditForce { path } => {
            if let Ok(buffer) = Buffer::open(&editor.fs, &path) {
                *editor.window_mut().buffer_mut() = buffer;
            } else {
                editor.message = Some(Err(format!("Failed to open {path}")));
            }
        }
        Command::Write { path } => {
            let fs = Arc::clone(&editor.fs);
            let result = if let Some(path) = path {
                editor.window_mut().buffer_mut().save_as(&fs, path)
            } else {
                editor.window_mut().buffer_mut().save(&fs)
            };
            if result.is_err() {
                editor.message = Some(Err(String::from("Failed to save")));
            }
        }
        Command::Quit { exit_code } => {
            if editor.window().buffer().is_modified().unwrap_or(false) {
                editor.message = Some(Err(String::from("Unsaved changes")));
            } else {
                editor.exit(exit_code.unwrap_or(0));
            }
        }
        Command::QuitForce { exit_code } => {
            editor.exit(exit_code.unwrap_or(0));
        }
        Command::WriteQuit { exit_code } => {
            let fs = Arc::clone(&editor.fs);
            if editor.window_mut().buffer_mut().save(&fs).is_ok() {
                editor.exit(exit_code.unwrap_or(0));
            } else {
                editor.message = Some(Err(String::from("Failed to save")));
            }
        }
        Command::Readonly => {
            let new_readonly = !editor.window().buffer().is_readonly();
            editor.window_mut().buffer_mut().set_readonly(new_readonly);
            if new_readonly {
                editor.message = Some(Ok(String::from("Buffer is now readonly")));
            } else {
                editor.message = Some(Ok(String::from("Buffer is no longer readonly")));
            }
        }
        Command::Panic => panic!(),
    }

    editor.mode = Mode::Normal(NormalMode::default());
}

enum Command {
    Echo { error: bool, message: Vec<String> },
    Edit { path: Utf8PathBuf },
    // TODO: Delete `edit!` once multiple buffers are supported.
    EditForce { path: Utf8PathBuf },
    Write { path: Option<Utf8PathBuf> },
    Quit { exit_code: Option<u8> },
    QuitForce { exit_code: Option<u8> },
    WriteQuit { exit_code: Option<u8> },
    Readonly,
    Panic,
}

fn parse_command(command: &Rope) -> anyhow::Result<Command> {
    use lexopt::prelude::*;

    let command = Cow::<str>::from(command);

    let Ok(args) = shell_words::split(&command) else {
        anyhow::bail!("Invalid command");
    };

    let Some(subcommand) = args.first() else {
        anyhow::bail!("Empty command");
    };

    let mut parser = lexopt::Parser::from_args(args[1..].iter().map(|s| s.as_str()));

    match subcommand.as_str() {
        "echo" => {
            let mut error = false;
            let mut message = Vec::new();
            while let Some(arg) = parser.next()? {
                match arg {
                    Long("error") => error = true,
                    Value(val) => message.push(val.string()?),
                    _ => return Err(arg.unexpected().into()),
                }
            }
            Ok(Command::Echo { error, message })
        }
        "edit" | "e" => {
            let path: Utf8PathBuf = parser
                .value()
                .map_err(|_| anyhow::anyhow!("Missing path"))?
                .string()?
                .into();
            Ok(Command::Edit { path })
        }
        "edit!" | "e!" => {
            let path: Utf8PathBuf = parser
                .value()
                .map_err(|_| anyhow::anyhow!("Missing path"))?
                .string()?
                .into();
            Ok(Command::EditForce { path })
        }
        "write" | "w" => {
            let path = match parser.next()? {
                Some(Value(val)) => Some(val.string()?.into()),
                Some(arg) => return Err(arg.unexpected().into()),
                None => None,
            };
            Ok(Command::Write { path })
        }
        "quit" | "q" => {
            let exit_code = match parser.next()? {
                Some(Value(val)) => Some(val.parse()?),
                Some(arg) => return Err(arg.unexpected().into()),
                None => None,
            };
            Ok(Command::Quit { exit_code })
        }
        "quit!" | "q!" => {
            let exit_code = match parser.next()? {
                Some(Value(val)) => Some(val.parse()?),
                None => None,
                Some(arg) => return Err(arg.unexpected().into()),
            };
            Ok(Command::QuitForce { exit_code })
        }
        "writequit" | "wq" => {
            let exit_code = match parser.next()? {
                Some(Value(val)) => Some(val.parse()?),
                None => None,
                Some(arg) => return Err(arg.unexpected().into()),
            };
            Ok(Command::WriteQuit { exit_code })
        }
        "readonly" | "ro" => Ok(Command::Readonly),
        "panic" => Ok(Command::Panic),
        _ => anyhow::bail!("Unknown command: {subcommand}"),
    }
}
