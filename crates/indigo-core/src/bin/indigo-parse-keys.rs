use clap::Parser as _;
use indigo_core::key::{Key, KeyCode, KeyModifiers, Keys};
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
    KeyCode,
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Keys => write!(f, "keys")?,
            Self::Key => write!(f, "key")?,
            Self::KeyModifiers => write!(f, "key-modifiers")?,
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
}

fn main() -> anyhow::Result<ExitCode> {
    let args = Args::parse();

    let input = io::read_to_string(io::stdin())?;

    let input = if let Type::Keys = args.r#type {
        &input
    } else {
        // Trim whitespace for parsers that can't handle it. Otherwise input on `stdin` that ends in
        // a newline (e.g. what `echo` outputs by default) will fail with a cryptic error.
        input.trim()
    };

    let exit_code = match args.r#type {
        Type::Keys => run::<Keys>(input, args.debug),
        Type::Key => run::<Key>(input, args.debug),
        Type::KeyModifiers => run::<KeyModifiers>(input, args.debug),
        Type::KeyCode => run::<KeyCode>(input, args.debug),
    };

    Ok(exit_code)
}

fn run<T>(input: &str, debug: bool) -> ExitCode
where
    T: FromStr + Debug + Display,
    <T as FromStr>::Err: Display,
{
    let result = input.parse::<T>();

    match &result {
        Ok(keys) if debug => println!("{keys:?}"),
        Ok(keys) => println!("{keys}"),
        Err(error) => println!("{error}"),
    }

    match &result {
        Ok(_) => ExitCode::SUCCESS,
        Err(_) => ExitCode::FAILURE,
    }
}
