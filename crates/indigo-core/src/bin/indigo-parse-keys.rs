use clap::Parser as _;
use indigo_core::key2::{Key, KeyCode, KeyModifier, KeyModifiers, Keys};
use std::{
    fmt::{self, Debug, Display},
    io,
    process::ExitCode,
    str::FromStr,
};

#[derive(clap::ValueEnum, Clone, Default)]
enum Type {
    #[default]
    Keys,
    Key,
    KeyModifiers,
    KeyModifier,
    KeyCode,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Keys => write!(f, "keys")?,
            Self::Key => write!(f, "key")?,
            Self::KeyModifiers => write!(f, "key-modifiers")?,
            Self::KeyModifier => write!(f, "key-modifier")?,
            Self::KeyCode => write!(f, "key-code")?,
        }
        Ok(())
    }
}

#[derive(clap::Parser)]
struct Args {
    /// Type to parse
    #[arg(long, default_value_t)]
    r#type: Type,

    /// Print parsed result with `Debug` instead of `Display`
    #[arg(long)]
    debug: bool,

    /// Parse each line separately
    #[arg(long)]
    lines: bool,
}

fn main() -> anyhow::Result<ExitCode> {
    let args = Args::parse();

    let runner = |input: &str| match args.r#type {
        Type::Keys => run::<Keys>(input, args.debug),
        Type::Key => run::<Key>(input, args.debug),
        Type::KeyModifiers => run::<KeyModifiers>(input, args.debug),
        Type::KeyModifier => run::<KeyModifier>(input, args.debug),
        Type::KeyCode => run::<KeyCode>(input, args.debug),
    };

    if args.lines {
        let mut exit_code = None;
        for line in io::stdin().lines() {
            if runner(&line?) != ExitCode::SUCCESS {
                exit_code = Some(ExitCode::FAILURE);
            }
        }
        Ok(exit_code.unwrap_or(ExitCode::SUCCESS))
    } else {
        let stdin = io::read_to_string(io::stdin())?;
        let exit_code = runner(&stdin);
        Ok(exit_code)
    }
}

fn run<T>(input: &str, debug: bool) -> ExitCode
where
    // TODO: Use `Display`.
    T: FromStr + Debug,
    <T as FromStr>::Err: Display,
{
    let result = input.parse::<T>();

    match &result {
        Ok(keys) if debug => println!("{keys:?}"),
        // TODO: Use `Display`.
        Ok(keys) => println!("{keys:?}"),
        Err(error) => println!("{error}"),
    }

    match &result {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}
