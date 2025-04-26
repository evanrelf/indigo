use bevy::{app::ScheduleRunnerPlugin, log::LogPlugin, prelude::*};
use std::time::Duration;

fn main() {
    App::new()
        .add_plugins(MinimalPlugins.set({
            let min_wait = Duration::from_secs_f64(1.0 / 60.0);
            ScheduleRunnerPlugin::run_loop(min_wait)
        }))
        .add_plugins(LogPlugin::default())
        .add_systems(Startup, hello_world)
        .add_systems(Update, tick)
        .run();
}

fn hello_world() {
    info!("Hello, world!");
}

fn tick() {
    info!("Tick");
}
