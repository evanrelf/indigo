use camino::Utf8PathBuf;

#[derive(clap::Parser)]
pub struct Cli {
    #[clap(subcommand)]
    pub command: Command,
}

#[derive(clap::Subcommand)]
pub enum Command {
    Quit(Quit),
    OpenBuffer(OpenBuffer),
    CloseBuffer(CloseBuffer),
}

#[derive(clap::Args)]
pub struct Quit;

#[derive(clap::Args)]
pub struct OpenBuffer {
    #[clap(value_parser)]
    pub path: Utf8PathBuf,
}

#[derive(clap::Args)]
pub struct CloseBuffer;
