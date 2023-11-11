use clap::Parser as _;

#[derive(Debug, clap::Parser, Eq, PartialEq)]
// TODO: This doesn't work? Help output in `parse` error isn't what I want.
#[clap(no_binary_name = true)]
pub struct Cli {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(Debug, clap::Subcommand, Eq, PartialEq)]
pub enum Command {
    Quit(Quit),
}

#[derive(Debug, clap::Args, Eq, PartialEq)]
pub struct Quit {
    pub exit_code: Option<u8>,
}

pub fn parse(command: &str) -> anyhow::Result<Cli> {
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
        assert_eq!(
            parse("q").unwrap().command,
            Command::Quit(Quit { exit_code: None })
        );
        assert_eq!(
            parse("quit").unwrap().command,
            Command::Quit(Quit { exit_code: None })
        );
        assert_eq!(
            parse("quit 42").unwrap().command,
            Command::Quit(Quit {
                exit_code: Some(42)
            })
        );
    }
}
