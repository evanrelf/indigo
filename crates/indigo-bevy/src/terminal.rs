use bevy::{
    prelude::*,
    tasks::{IoTaskPool, futures_lite::StreamExt as _},
};

#[derive(Deref, DerefMut, Resource)]
pub struct Terminal(pub ratatui::DefaultTerminal);

#[derive(Resource)]
pub struct TerminalEventReader(flume::Receiver<TerminalEvent>);

impl Drop for Terminal {
    fn drop(&mut self) {
        ratatui::restore();
    }
}

#[derive(Deref, DerefMut, Event)]
pub struct TerminalEvent(pub crossterm::event::Event);

pub struct TuiPlugin;

impl TuiPlugin {
    pub fn reader_system(mut commands: Commands) {
        use crossterm::event::EventStream;

        let (sender, receiver) = flume::bounded(32);
        commands.insert_resource(TerminalEventReader(receiver));

        let task_pool = IoTaskPool::get();
        let task = task_pool.spawn(async move {
            let mut event_stream = EventStream::new();
            while let Some(event) = event_stream.next().await.map(|result| result.unwrap()) {
                if sender.send_async(TerminalEvent(event)).await.is_err() {
                    break;
                }
            }
        });
        task.detach();
    }

    pub fn writer_system(reader: Res<TerminalEventReader>, mut writer: EventWriter<TerminalEvent>) {
        writer.write_batch(reader.0.try_iter());
    }
}

impl Plugin for TuiPlugin {
    fn build(&self, app: &mut App) {
        let terminal = ratatui::init();
        app.insert_resource(Terminal(terminal))
            .add_event::<TerminalEvent>()
            .add_systems(Startup, Self::reader_system)
            .add_systems(PreUpdate, Self::writer_system);
    }
}
