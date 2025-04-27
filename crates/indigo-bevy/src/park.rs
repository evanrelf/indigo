use bevy::prelude::*;
use std::time::Duration;

pub struct ParkPlugin;

#[derive(Default, Resource)]
pub struct ParkSettings {
    pub duration: Option<Duration>,
}

struct Parker(parking::Parker);

#[derive(Deref, Resource)]
pub struct Unparker(pub parking::Unparker);

impl ParkPlugin {
    fn park_system(parker: NonSend<Parker>, settings: Res<ParkSettings>) {
        if let Some(duration) = settings.duration {
            parker.0.park_timeout(duration);
        } else {
            parker.0.park();
        }
    }
}

impl Plugin for ParkPlugin {
    fn build(&self, app: &mut App) {
        let (parker, unparker) = parking::pair();
        app.insert_resource(ParkSettings::default())
            .insert_non_send_resource(Parker(parker))
            .insert_resource(Unparker(unparker))
            // TODO: How to make this the last of the last?
            .add_systems(Last, Self::park_system);
    }
}
