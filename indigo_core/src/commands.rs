use camino::Utf8PathBuf;
use clap::Parser as _;

#[derive(clap::Parser, Debug)]
#[clap(no_binary_name = true)]
pub struct Cli {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(clap::Subcommand, Debug, PartialEq)]
pub enum Command {
    Nop,

    Quit(Quit),

    #[clap(name = "open")]
    OpenBuffer(OpenBuffer),

    #[clap(name = "close")]
    CloseBuffer,
}

#[derive(clap::Args, Debug, PartialEq)]
pub struct Quit {
    #[clap(value_parser)]
    pub exit_code: Option<u8>,
}

#[derive(clap::Args, Debug, PartialEq)]
pub struct OpenBuffer {
    #[clap(value_parser)]
    pub path: Utf8PathBuf,
}

pub fn parse(command: &str) -> Result<Cli, anyhow::Error> {
    let mut words = shell_words::split(command)?;
    if let Some(command) = words.get(0) {
        #[allow(clippy::single_match)]
        match command.as_str() {
            "q" => words[0] = String::from("quit"),
            _ => {}
        }
    }
    let cli = Cli::try_parse_from(words)?;
    Ok(cli)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        macro_rules! case {
            ($string:literal, $command:expr) => {
                assert_eq!(parse($string).unwrap().command, $command)
            };
        }

        case!("nop", Command::Nop);

        case!("quit", Command::Quit(Quit { exit_code: None }));

        case!(
            "open foo.txt",
            Command::OpenBuffer(OpenBuffer {
                path: Utf8PathBuf::from("foo.txt")
            })
        );

        case!("close", Command::CloseBuffer);
    }
}
