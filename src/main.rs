use clap::Parser as _;

#[derive(Debug, clap::Parser)]
struct Args {}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    println!("{args:?}");

    Ok(())
}
