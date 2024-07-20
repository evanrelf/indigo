use std::{fmt::Display, str::FromStr};
use winnow::{combinator::alt, prelude::*};

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
pub enum Mode {
    #[default]
    Normal,
    Insert,
}

impl Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Normal => write!(f, "normal"),
            Self::Insert => write!(f, "insert"),
        }
    }
}

impl FromStr for Mode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        mode.parse(s).map_err(|e| e.to_string())
    }
}

fn mode(input: &mut &str) -> PResult<Mode> {
    alt((
        alt(("normal", "n")).value(Mode::Normal),
        alt(("insert", "i")).value(Mode::Insert),
    ))
    .parse_next(input)
}
