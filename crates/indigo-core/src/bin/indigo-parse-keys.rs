use clap::Parser as _;
use indigo_core::key2::{Key, KeyCode, KeyModifier, KeyModifiers, Keys};
use std::{
    fmt::{self, Debug, Display},
    io,
    process::ExitCode,
    str::FromStr,
};

// $ watchexec --quiet --clear -- cat keys \| cargo run --quiet --bin indigo-parse-keys

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
            Type::Keys => write!(f, "keys")?,
            Type::Key => write!(f, "key")?,
            Type::KeyModifiers => write!(f, "key-modifiers")?,
            Type::KeyModifier => write!(f, "key-modifier")?,
            Type::KeyCode => write!(f, "key-code")?,
        }
        Ok(())
    }
}

#[derive(clap::Parser)]
struct Args {
    #[arg(long, default_value_t)]
    r#type: Type,

    #[arg(long)]
    debug: bool,
}

fn main() -> anyhow::Result<ExitCode> {
    let args = Args::parse();

    match args.r#type {
        Type::Keys => run::<Keys>(args.debug),
        Type::Key => run::<Key>(args.debug),
        Type::KeyModifiers => run::<KeyModifiers>(args.debug),
        Type::KeyModifier => run::<KeyModifier>(args.debug),
        Type::KeyCode => run::<KeyCode>(args.debug),
    }
}

fn run<T>(debug: bool) -> anyhow::Result<ExitCode>
where
    // TODO: Use `Display`.
    T: FromStr + Debug,
    <T as FromStr>::Err: Display,
{
    let input = io::read_to_string(io::stdin())?;

    let result = input.parse::<T>();

    match &result {
        Ok(keys) if debug => println!("{keys:?}"),
        // TODO: Use `Display`.
        Ok(keys) => println!("{keys:?}"),
        Err(error) => println!("{error}"),
    }

    match &result {
        Ok(_) => Ok(ExitCode::SUCCESS),
        Err(_) => Ok(ExitCode::FAILURE),
    }
}
