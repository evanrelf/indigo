use crate::{buffer::Buffer, editor::Editor, mode::prompt::enter_prompt_mode, window::WindowState};
use anyhow::anyhow;
use camino::Utf8PathBuf;
use std::sync::Arc;

pub fn enter_command_mode(editor: &mut Editor) {
    enter_prompt_mode(editor, "", |editor, input| match parse_command(input) {
        Ok(command) => handle_command(editor, command),
        Err(error) => editor.message = Some(Err(error.to_string())),
    });
}

#[derive(Clone)]
enum Command {
    Echo { error: bool, message: Vec<String> },
    Edit { path: Utf8PathBuf },
    Write { path: Option<Utf8PathBuf> },
    Quit { exit_code: Option<u8> },
    QuitForce { exit_code: Option<u8> },
    WriteQuit { exit_code: Option<u8> },
    Readonly,
    Panic,
}

fn parse_command(command: &str) -> anyhow::Result<Command> {
    use bpaf::{ParseFailure, Parser, construct, long, positional, pure};

    let echo = {
        let error = long("error").switch();
        let message = positional("MESSAGE").many();
        construct!(Command::Echo { error, message })
            .to_options()
            .command("echo")
    };

    let edit = {
        let path = positional("PATH");
        construct!(Command::Edit { path })
            .to_options()
            .command("edit")
            .long("e")
    };

    let write = {
        let path = positional("PATH").optional();
        construct!(Command::Write { path })
            .to_options()
            .command("write")
            .long("w")
    };

    let quit = {
        let exit_code = positional("EXIT_CODE").optional();
        construct!(Command::Quit { exit_code })
            .to_options()
            .command("quit")
            .long("q")
    };

    let quit_force = {
        let exit_code = positional("EXIT_CODE").optional();
        construct!(Command::QuitForce { exit_code })
            .to_options()
            .command("quit!")
            .long("q!")
    };

    let write_quit = {
        let exit_code = positional("EXIT_CODE").optional();
        construct!(Command::WriteQuit { exit_code })
            .to_options()
            .command("writequit")
            .long("wq")
    };

    let readonly = pure(Command::Readonly)
        .to_options()
        .command("readonly")
        .long("ro");

    let panic = pure(Command::Panic).to_options().command("panic");

    let parser = construct!([
        echo, edit, write, quit, quit_force, write_quit, readonly, panic
    ])
    .to_options();

    let Ok(args) = shell_words::split(command) else {
        anyhow::bail!("Invalid command");
    };

    parser.run_inner(args.as_slice()).map_err(|failure| {
        let message = match failure {
            ParseFailure::Stderr(doc) => doc.monochrome(true),
            ParseFailure::Stdout(doc, full) => doc.monochrome(full),
            ParseFailure::Completion(s) => s,
        };
        anyhow!("{message}")
    })
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
            // TODO: Edit existing buffer for path if one exists.
            if let Ok(buffer) = Buffer::open(&editor.fs, &path) {
                // In Kakoune, windows are inextricably linked to buffers, so changing buffer means
                // changing window, creating a new one if necessary.
                // https://github.com/mawww/kakoune/blob/afc035ac0e17a6280edec511dfe8da1c0dd565ab/doc/pages/scopes.asciidoc?plain=1#L29-L30
                let buffer_key = editor.insert_buffer(buffer);
                let window_state =
                    WindowState::new(buffer_key, editor.get_buffer(buffer_key).unwrap());
                let window_key = editor.insert_window(window_state);
                assert!(editor.focus_window(window_key));
                // TODO: Delete previous window. It's no longer focused, so (at the time of writing)
                // that means it's not displayed, it's just hanging out unused in memory.
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
            let new_readonly = !editor.focused_buffer().text.readonly;
            editor.focused_buffer_mut().text.readonly = new_readonly;
            if new_readonly {
                editor.message = Some(Ok(String::from("Buffer is now readonly")));
            } else {
                editor.message = Some(Ok(String::from("Buffer is no longer readonly")));
            }
        }
        Command::Panic => panic!(),
    }
}
