use bevy::{
    app::MainScheduleOrder,
    ecs::schedule::{ExecutorKind, ScheduleLabel},
    prelude::*,
};
use std::time::Duration;

/// Park the main thread to reduce CPU utilization. Explicitly unpark from background threads when
/// there is work to do, e.g. user input, file I/O, or other external stimulus.
pub struct ParkPlugin;

impl ParkPlugin {
    pub fn system(
        mut first_frame: Local<bool>,
        mut frames: Local<usize>,
        parker: NonSend<Parker>,
        settings: Res<ParkSettings>,
    ) {
        if *first_frame {
            *frames = settings.unparked_frames;
            *first_frame = false;
        }

        if *frames == 0 {
            if let Some(park_duration) = settings.park_duration {
                parker.park_timeout(park_duration);
            } else {
                parker.park();
            }
            *frames = settings.unparked_frames;
            return;
        }

        *frames -= 1;
    }

    pub fn is_enabled(settings: Res<ParkSettings>) -> bool {
        settings.enabled
    }
}

impl Plugin for ParkPlugin {
    fn build(&self, app: &mut App) {
        app.insert_resource(ParkSettings::default());

        let (parker, unparker) = parking::pair();
        app.insert_non_send_resource(Parker(parker));
        app.insert_resource(Unparker(unparker));

        let mut schedule = Schedule::new(Park);
        schedule.set_executor_kind(ExecutorKind::SingleThreaded);
        app.add_schedule(schedule);
        let mut main_schedule_order = app.world_mut().resource_mut::<MainScheduleOrder>();
        main_schedule_order.insert_after(Last, Park);

        app.add_systems(Park, Self::system.run_if(Self::is_enabled));
    }
}

#[derive(Resource)]
pub struct ParkSettings {
    /// Whether the plugin is enabled.
    pub enabled: bool,

    /// Whether to park indefinitely, or wake after a fixed duration.
    pub park_duration: Option<Duration>,

    /// How many additional frames to run before parking again.
    pub unparked_frames: usize,
}

impl Default for ParkSettings {
    fn default() -> Self {
        Self {
            enabled: true,

            park_duration: None,

            // 0 is almost always too few. Events are only seen by systems running later in the same
            // frame, so e.g. emitting an `AppExit` event likely won't cause the app to exit until
            // woken again later.
            //
            // 1 might be barely enough (addresses the aformentioned issue), but still cuts it very
            // close.
            //
            // 10 is generous, but still far more economical than the normal behavior without this
            // plugin enabled.
            unparked_frames: 10,
        }
    }
}

/// Parking occurs in its own `Park` schedule, running after the `Last` schedule.
#[derive(Clone, Debug, Eq, Hash, PartialEq, ScheduleLabel)]
pub struct Park;

#[derive(Deref)]
pub struct Parker(pub parking::Parker);

#[derive(Deref, Resource)]
pub struct Unparker(pub parking::Unparker);
