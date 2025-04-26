use bevy::{app::ScheduleRunnerPlugin, prelude::*};
use clap::Parser as _;
use std::time::Duration;

#[derive(clap::Parser)]
struct Args {}

fn main() {
    let _args = Args::parse();

    App::new()
        .add_plugins(MinimalPlugins.set({
            let min_wait = Duration::from_secs_f64(1.0 / 60.0);
            ScheduleRunnerPlugin::run_loop(min_wait)
        }))
        .run();
}
