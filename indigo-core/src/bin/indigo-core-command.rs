use clap::Parser as _;
use indigo_core::command::Command;

fn main() {
    let command = Command::parse();
    println!("{command:#?}");
}
