use clap::Parser as _;

#[derive(Debug, clap::Parser, Eq, PartialEq)]
// TODO: This doesn't work? Help output in `parse` error isn't what I want.
#[clap(no_binary_name = true)]
pub enum Command {
    #[command(alias = "q")]
    Quit(Quit),
}

#[derive(Debug, clap::Args, Eq, PartialEq)]
pub struct Quit {
    pub exit_code: Option<u8>,
}

pub fn parse(input: &str) -> anyhow::Result<Command> {
    let words = shell_words::split(input)?;
    let command = Command::try_parse_from(words)?;
    Ok(command)
}

// TODO: Add `help` function that prints help text, similar to the thing I made in Haskell here:
// https://github.com/evanrelf/indigo/blob/843d98fcc6c133efad22038703209a76e739a127/src/Indigo/Core/Command.hs#L37-L53

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(parse("q").unwrap(), Command::Quit(Quit { exit_code: None }));
        assert_eq!(
            parse("quit").unwrap(),
            Command::Quit(Quit { exit_code: None })
        );
        assert_eq!(
            parse("quit 42").unwrap(),
            Command::Quit(Quit {
                exit_code: Some(42)
            })
        );
    }
}
