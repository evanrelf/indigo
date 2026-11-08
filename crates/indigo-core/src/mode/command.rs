use crate::{buffer::Buffer, client::ClientMut, mode::prompt, window::WindowState};
use anyhow::anyhow;
use bpaf::{OptionParser, ParseFailure, Parser, choice, construct, long, positional, pure};
use camino::Utf8PathBuf;
use std::borrow::Cow;

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
        Err(error) => client.set_error(error.to_string()),
    });
}

struct Spec {
    name: &'static str,
    aliases: &'static [&'static str],
    /// Parses the command's arguments. The name and aliases are applied by `parse_command`.
    parser: fn() -> OptionParser<Command>,
}

const COMMANDS: &[Spec] = &[
    Spec {
        name: "echo",
        aliases: &[],
        parser: || {
            let error = long("error").switch();
            let message = positional("MESSAGE").many();
            construct!(Command::Echo { error, message }).to_options()
        },
    },
    Spec {
        name: "edit",
        aliases: &["e"],
        parser: || {
            let path = positional("PATH");
            construct!(Command::Edit { path }).to_options()
        },
    },
    Spec {
        name: "write",
        aliases: &["w"],
        parser: || {
            let path = positional("PATH").optional();
            construct!(Command::Write { path }).to_options()
        },
    },
    Spec {
        name: "quit",
        aliases: &["q"],
        parser: || {
            let exit_code = positional("EXIT_CODE").optional();
            construct!(Command::Quit { exit_code }).to_options()
        },
    },
    Spec {
        name: "quit!",
        aliases: &["q!"],
        parser: || {
            let exit_code = positional("EXIT_CODE").optional();
            construct!(Command::QuitForce { exit_code }).to_options()
        },
    },
    Spec {
        name: "writequit",
        aliases: &["wq"],
        parser: || {
            let exit_code = positional("EXIT_CODE").optional();
            construct!(Command::WriteQuit { exit_code }).to_options()
        },
    },
    Spec {
        name: "readonly",
        aliases: &["ro"],
        parser: || pure(Command::Readonly).to_options(),
    },
    Spec {
        name: "panic",
        aliases: &[],
        parser: || pure(Command::Panic).to_options(),
    },
];

fn parse_command(command: &str) -> anyhow::Result<Command> {
    let parser = choice(COMMANDS.iter().map(|spec| {
        let mut parser = (spec.parser)().command(spec.name);
        for alias in spec.aliases {
            parser = parser.long(alias);
        }
        parser.boxed()
    }))
    .to_options();

    let Ok(args) = shell_words::split(command) else {
        anyhow::bail!("Invalid command");
    };

    if args.is_empty() {
        return Ok(Command::Nop);
    }

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
                client.set_error(message.join(" "));
            } else {
                client.set_message(message.join(" "));
            }
        }
        Command::Edit { path } => {
            // TODO: Edit existing buffer for path if one exists.
            let absolute_path = client.absolute_path(path);
            match Buffer::open(client.fs(), &absolute_path) {
                Ok(buffer) => {
                    // In Kakoune, windows are inextricably linked to buffers, so changing buffer
                    // means changing window, creating a new one if necessary.
                    // https://github.com/mawww/kakoune/blob/afc035ac0e17a6280edec511dfe8da1c0dd565ab/doc/pages/scopes.asciidoc?plain=1#L29-L30
                    let buffer_key = client.session_mut().insert_buffer(buffer);
                    let window_state = WindowState::new(buffer_key);
                    let window_key = client.insert_window(window_state);
                    assert!(client.focus_window(window_key));
                    // TODO: Delete previous window. It's no longer focused, so (at the time of
                    // writing) that means it's not displayed, it's just hanging out unused in
                    // memory.
                }
                Err(error) => {
                    let path = client.display_path(&absolute_path);
                    client.set_error(format!("Failed to open {path}: {error:#}"));
                }
            }
        }
        Command::Write { path } => {
            let fs = client.fs();
            let absolute_path = path.as_ref().map(|path| client.absolute_path(path));
            let result = if let Some(absolute_path) = &absolute_path {
                client.focused_buffer_mut().save_as(fs, absolute_path)
            } else {
                client.focused_buffer_mut().save(fs)
            };
            if let Err(error) = result {
                let path = absolute_path
                    .as_deref()
                    .or_else(|| client.focused_buffer().path())
                    .map_or(Cow::Borrowed("a scratch buffer"), |path| {
                        client.display_path(path)
                    });
                let error = format!("Failed to save {path}: {error:#}");
                client.set_error(error);
            }
        }
        Command::Quit { exit_code } => {
            let mut modified = None;
            for (_, buffer) in client.session().buffers() {
                if buffer.is_modified().unwrap_or(false) {
                    modified = Some(
                        buffer
                            .path()
                            .map_or(Cow::Borrowed("a scratch buffer"), |path| {
                                client.display_path(path)
                            }),
                    );
                    break;
                }
            }
            if let Some(modified) = modified {
                let error = format!("Unsaved changes in {modified}");
                client.set_error(error);
            } else {
                client.exit(exit_code.unwrap_or(0));
            }
        }
        Command::QuitForce { exit_code } => {
            client.exit(exit_code.unwrap_or(0));
        }
        Command::WriteQuit { exit_code } => {
            let fs = client.fs();
            match client.focused_buffer_mut().save(fs) {
                Ok(()) => client.exit(exit_code.unwrap_or(0)),
                Err(error) => {
                    let path = client
                        .focused_buffer()
                        .path()
                        .map_or(Cow::Borrowed("a scratch buffer"), |path| {
                            client.display_path(path)
                        });
                    let error = format!("Failed to save {path}: {error:#}");
                    client.set_error(error);
                }
            }
        }
        Command::Readonly => {
            let new_readonly = !client.focused_buffer().text.readonly;
            client.focused_buffer_mut().text.readonly = new_readonly;
            if new_readonly {
                client.set_message("Buffer is now readonly");
            } else {
                client.set_message("Buffer is no longer readonly");
            }
        }
        Command::Panic => panic!(),
    }
}
