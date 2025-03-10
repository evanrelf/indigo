use clap::Parser as _;
use indigo_core::{
    action::{Action, handle_action},
    prelude::*,
};
use std::{panic, thread};

#[derive(clap::Parser)]
struct Args {
    #[clap(long)]
    seed: Option<u64>,
}

fn main() {
    let args = Args::parse();

    if let Some(seed) = args.seed {
        sim(seed);
        return;
    }

    let n = thread::available_parallelism().unwrap().get();

    let mut threads = Vec::with_capacity(n);

    for _ in 0..n {
        let seed = fastrand::u64(..);
        let handle = thread::spawn(move || sim(seed));
        threads.push((seed, handle));
    }

    for (seed, handle) in threads {
        if let Err(panic) = handle.join() {
            println!("Discovered a panic! (seed: {seed})");
            panic::resume_unwind(panic);
        }
    }
}

fn sim(seed: u64) {
    let mut raw_data = [0u8; 1024];
    fastrand::Rng::with_seed(seed).fill(&mut raw_data);
    let mut unstructured = arbitrary::Unstructured::new(&raw_data);

    let rope = Rope::from_str(unstructured.arbitrary::<&str>().unwrap());
    let mut editor = Editor::from(Buffer::from(rope));

    loop {
        let action = unstructured.arbitrary::<Action>().unwrap();
        handle_action(&mut editor, action);
    }
}
