use clap::Parser as _;

#[derive(clap::Parser)]
struct Args {}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let _args = Args::parse();

    println!("Hello, world!");

    Ok(())
}
