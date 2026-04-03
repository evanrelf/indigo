use crate::{buffer::Buffer, editor::Editor, mode::prompt::enter_prompt_mode};
use camino::Utf8PathBuf;
use std::sync::Arc;

pub fn enter_command_mode(editor: &mut Editor) {
    enter_prompt_mode(editor, "", |editor, input| match parse_command(input) {
        Ok(command) => handle_command(editor, command),
        Err(error) => editor.message = Some(Err(error.to_string())),
    });
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

fn parse_command(command: &str) -> anyhow::Result<Command> {
    use lexopt::prelude::*;

    let Ok(args) = shell_words::split(command) else {
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

fn handle_command(editor: &mut Editor, command: Command) {
    match command {
        Command::Echo { error, message } => {
            if error {
                editor.message = Some(Err(message.join(" ")));
            } else {
                editor.message = Some(Ok(message.join(" ")));
            }
        }
        Command::Edit { path } => {
            if editor.focused_buffer().is_modified().unwrap_or(false) {
                editor.message = Some(Err(String::from("Unsaved changes")));
            } else if let Ok(buffer) = Buffer::open(&editor.fs, &path) {
                *editor.focused_buffer_mut() = buffer;
            } else {
                editor.message = Some(Err(format!("Failed to open {path}")));
            }
        }
        Command::EditForce { path } => {
            if let Ok(buffer) = Buffer::open(&editor.fs, &path) {
                *editor.focused_buffer_mut() = buffer;
            } else {
                editor.message = Some(Err(format!("Failed to open {path}")));
            }
        }
        Command::Write { path } => {
            let fs = Arc::clone(&editor.fs);
            let result = if let Some(path) = path {
                editor.focused_buffer_mut().save_as(&fs, path)
            } else {
                editor.focused_buffer_mut().save(&fs)
            };
            if result.is_err() {
                editor.message = Some(Err(String::from("Failed to save")));
            }
        }
        Command::Quit { exit_code } => {
            let mut modified = None;
            for (_, buffer) in editor.buffers() {
                if buffer.is_modified().unwrap_or(false) {
                    modified = Some(
                        buffer
                            .path()
                            .map_or("a scratch buffer", |path| path.as_str()),
                    );
                    break;
                }
            }
            if let Some(modified) = modified {
                editor.message = Some(Err(format!("Unsaved changes in {modified}")));
            } else {
                editor.exit(exit_code.unwrap_or(0));
            }
        }
        Command::QuitForce { exit_code } => {
            editor.exit(exit_code.unwrap_or(0));
        }
        Command::WriteQuit { exit_code } => {
            let fs = Arc::clone(&editor.fs);
            if editor.focused_buffer_mut().save(&fs).is_ok() {
                editor.exit(exit_code.unwrap_or(0));
            } else {
                editor.message = Some(Err(String::from("Failed to save")));
            }
        }
        Command::Readonly => {
            let new_readonly = !editor.focused_buffer().is_readonly();
            editor.focused_buffer_mut().set_readonly(new_readonly);
            if new_readonly {
                editor.message = Some(Ok(String::from("Buffer is now readonly")));
            } else {
                editor.message = Some(Ok(String::from("Buffer is no longer readonly")));
            }
        }
        Command::Panic => panic!(),
    }
}
