use bevy::prelude::*;
use std::time::Duration;

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

            // Infrequent enough to be problematic if you're expecting real-time responsiveness, in
            // which case you should configure a system off the main thread to unpark.
            //
            // But regular enough for time-insensitive polling tasks, and better than grinding to a
            // complete halt in the absence of external unparking.
            park_duration: Some(Duration::from_secs(1)),

            // 0 is almost always too few. Events are only seen by systems running later in the same
            // frame, so e.g. emitting an `AppExit` event likely won't cause the app to exit until
            // woken again later.
            //
            // 1 might be barely enough (addresses the aformentioned issue), but still cuts it very
            // close.
            //
            // 30 is generous, but still far more economical than the normal behavior without this
            // plugin enabled.
            unparked_frames: 30,
        }
    }
}

pub struct Parker(parking::Parker);

#[derive(Deref, Resource)]
pub struct Unparker(pub parking::Unparker);

/// Park the main thread to reduce CPU utilization. Explicitly unpark from background threads when
/// there is work to do, e.g. user input, file I/O, or other external stimulus.
pub struct ParkPlugin;

impl ParkPlugin {
    pub fn park_system(
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
                parker.0.park_timeout(park_duration);
            } else {
                parker.0.park();
            }
            *frames = settings.unparked_frames;
            return;
        }

        *frames -= 1;
    }

    fn is_enabled(settings: Res<ParkSettings>) -> bool {
        settings.enabled
    }
}

impl Plugin for ParkPlugin {
    fn build(&self, app: &mut App) {
        let (parker, unparker) = parking::pair();
        app.insert_resource(ParkSettings::default())
            .insert_non_send_resource(Parker(parker))
            .insert_resource(Unparker(unparker))
            // TODO: How to make this the last of the last?
            .add_systems(Last, Self::park_system.run_if(Self::is_enabled));
    }
}
