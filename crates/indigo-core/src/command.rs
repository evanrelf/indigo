use clap::Parser as _;

#[derive(Debug, clap::Parser, Eq, PartialEq)]
pub enum Command {
    #[command(alias = "q")]
    Quit(Quit),
}

#[derive(Debug, clap::Args, Eq, PartialEq)]
pub struct Quit {
    pub exit_code: Option<u8>,
}

pub fn parse(input: &str) -> anyhow::Result<Command> {
    // TODO: Remove requirement for command name (`indigo-core` in this case), while maintaining
    // support for separate `indigo-core-command` bin.
    let words = shell_words::split(&format!("indigo-core {input}"))?;
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
