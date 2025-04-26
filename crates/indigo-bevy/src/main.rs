use bevy::{app::ScheduleRunnerPlugin, prelude::*};
use std::time::Duration;

fn main() {
    App::new()
        .add_plugins(MinimalPlugins.set({
            let min_wait = Duration::from_secs_f64(1.0 / 60.0);
            ScheduleRunnerPlugin::run_loop(min_wait)
        }))
        .add_systems(Startup, hello_world)
        .add_systems(Update, tick)
        .run();
}

fn hello_world() {
    println!("Hello, world!");
}

fn tick() {
    println!("Tick");
}
