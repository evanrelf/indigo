use crate::{buffer::Buffer, client::ClientMut, mode::prompt, window::WindowState};
use anyhow::anyhow;
use camino::Utf8PathBuf;
use std::borrow::Cow;

#[cfg(feature = "arbitrary")]
use arbitrary::Arbitrary;

pub type Action = Command;

#[derive(Clone, Debug)]
pub enum Command {
    Nop,
    Echo { error: bool, message: Vec<String> },
    Edit { path: Utf8PathBuf },
    Write { path: Option<Utf8PathBuf> },
    Quit { exit_code: Option<u8> },
    QuitForce { exit_code: Option<u8> },
    WriteQuit { exit_code: Option<u8> },
    Readonly,
    Panic,
}

#[cfg(feature = "arbitrary")]
impl<'a> Arbitrary<'a> for Command {
    fn arbitrary(_u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        // TODO: Uhhh I don't know what's most useful here
        Ok(Self::Nop)
    }
}

pub fn handle_actions(client: &mut ClientMut, actions: &[Action]) {
    for action in actions {
        handle_action(client, action);
    }
}

pub fn handle_action(client: &mut ClientMut, action: &Action) {
    handle_command(client, action);
}

pub fn enter_command_mode(client: &mut ClientMut) {
    prompt::enter(client, "", |client, input| match parse_command(input) {
        Ok(command) => handle_command(client, &command),
        Err(error) => client.message = Some(Err(Cow::Owned(error.to_string()))),
    });
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

fn handle_command(client: &mut ClientMut, command: &Command) {
    match command {
        Command::Nop => {}
        Command::Echo { error, message } => {
            if *error {
                client.message = Some(Err(Cow::Owned(message.join(" "))));
            } else {
                client.message = Some(Ok(Cow::Owned(message.join(" "))));
            }
        }
        Command::Edit { path } => {
            // TODO: Edit existing buffer for path if one exists.
            if let Ok(buffer) = Buffer::open(client.fs(), path) {
                // In Kakoune, windows are inextricably linked to buffers, so changing buffer means
                // changing window, creating a new one if necessary.
                // https://github.com/mawww/kakoune/blob/afc035ac0e17a6280edec511dfe8da1c0dd565ab/doc/pages/scopes.asciidoc?plain=1#L29-L30
                let buffer_key = client.session_mut().insert_buffer(buffer);
                let window_state = WindowState::new(buffer_key);
                let window_key = client.insert_window(window_state);
                assert!(client.focus_window(window_key));
                // TODO: Delete previous window. It's no longer focused, so (at the time of writing)
                // that means it's not displayed, it's just hanging out unused in memory.
            } else {
                client.message = Some(Err(Cow::Owned(format!("Failed to open {path}"))));
            }
        }
        Command::Write { path } => {
            let fs = client.fs();
            let result = if let Some(path) = path {
                client.focused_buffer_mut().save_as(fs, path)
            } else {
                client.focused_buffer_mut().save(fs)
            };
            if result.is_err() {
                client.message = Some(Err(Cow::Borrowed("Failed to save")));
            }
        }
        Command::Quit { exit_code } => {
            let mut modified = None;
            for (_, buffer) in client.session().buffers() {
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
                client.message = Some(Err(Cow::Owned(format!("Unsaved changes in {modified}"))));
            } else {
                client.exit(exit_code.unwrap_or(0));
            }
        }
        Command::QuitForce { exit_code } => {
            client.exit(exit_code.unwrap_or(0));
        }
        Command::WriteQuit { exit_code } => {
            let fs = client.fs();
            if client.focused_buffer_mut().save(fs).is_ok() {
                client.exit(exit_code.unwrap_or(0));
            } else {
                client.message = Some(Err(Cow::Borrowed("Failed to save")));
            }
        }
        Command::Readonly => {
            let new_readonly = !client.focused_buffer().text.readonly;
            client.focused_buffer_mut().text.readonly = new_readonly;
            if new_readonly {
                client.message = Some(Ok(Cow::Borrowed("Buffer is now readonly")));
            } else {
                client.message = Some(Ok(Cow::Borrowed("Buffer is no longer readonly")));
            }
        }
        Command::Panic => panic!(),
    }
}
