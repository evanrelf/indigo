use camino::Utf8PathBuf;
use clap::Parser as _;
use indigo_core::Keys;

#[derive(Debug, clap::Parser)]
struct Args {
    file: Option<Utf8PathBuf>,

    #[arg(long, short)]
    keys: Keys,
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    #[cfg(debug_assertions)]
    if std::env::var("RUST_BACKTRACE").is_err() {
        std::env::set_var("RUST_BACKTRACE", "1");
    }

    tracing_subscriber::fmt::init();

    let args = Args::parse();

    // TODO: Once `indigo-cli` has an `Editor`, check that it's valid in debug mode.
    // #[cfg(debug_assertions)]
    // editor.assert_valid();

    println!("file: {:?}", args.file);
    println!("keys: {}", args.keys);
}
