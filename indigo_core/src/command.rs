use camino::Utf8PathBuf;
use clap::Parser as _;

#[derive(clap::Parser)]
pub struct Cli {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(clap::Subcommand, Debug, PartialEq)]
pub enum Command {
    Quit(Quit),

    #[clap(name = "open")]
    OpenBuffer(OpenBuffer),

    #[clap(name = "close")]
    CloseBuffer(CloseBuffer),
}

#[derive(clap::Args, Debug, PartialEq)]
pub struct Quit;

#[derive(clap::Args, Debug, PartialEq)]
pub struct OpenBuffer {
    #[clap(value_parser)]
    pub path: Utf8PathBuf,
}

#[derive(clap::Args, Debug, PartialEq)]
pub struct CloseBuffer;

pub fn parse(command: &str) -> Result<Cli, anyhow::Error> {
    let words = shell_words::split(command)?;
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
                assert_eq!(
                    parse(&(String::from("cli ") + $string)).unwrap().command,
                    $command
                )
            };
        }

        case!("quit", Command::Quit(Quit));

        case!(
            "open foo.txt",
            Command::OpenBuffer(OpenBuffer {
                path: Utf8PathBuf::from("foo.txt")
            })
        );

        case!("close", Command::CloseBuffer(CloseBuffer));
    }
}
