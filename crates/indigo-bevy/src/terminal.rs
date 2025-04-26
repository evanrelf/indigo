use bevy::{
    prelude::*,
    tasks::{IoTaskPool, futures_lite::StreamExt as _},
};

#[derive(Deref, DerefMut, Resource)]
pub struct Terminal(pub ratatui::DefaultTerminal);

#[derive(Resource)]
pub struct TerminalEventReader(flume::Receiver<TerminalEvent>);

impl Terminal {
    pub fn spawn_event_reader_system(mut commands: Commands) {
        use crossterm::event::EventStream;

        let (sender, receiver) = flume::bounded(32);
        commands.insert_resource(TerminalEventReader(receiver));

        let task_pool = IoTaskPool::get();
        let task = task_pool.spawn(async move {
            let mut event_stream = EventStream::new();
            loop {
                let Some(result) = event_stream.next().await else {
                    // No more events
                    break;
                };
                let event = result.unwrap(); // Panic on I/O error
                if sender.send_async(TerminalEvent(event)).await.is_err() {
                    // Channel is closed
                    break;
                }
            }
        });
        task.detach();
    }

    pub fn write_event_system(
        reader: Res<TerminalEventReader>,
        mut writer: EventWriter<TerminalEvent>,
    ) {
        writer.write_batch(reader.0.try_iter());
    }
}

impl Drop for Terminal {
    fn drop(&mut self) {
        ratatui::restore();
    }
}

#[derive(Deref, DerefMut, Event)]
pub struct TerminalEvent(pub crossterm::event::Event);

pub struct TerminalPlugin;

impl Plugin for TerminalPlugin {
    fn build(&self, app: &mut App) {
        let terminal = ratatui::init();
        app.insert_resource(Terminal(terminal))
            .add_event::<TerminalEvent>()
            .add_systems(Startup, Terminal::spawn_event_reader_system)
            .add_systems(PreUpdate, Terminal::write_event_system);
    }
}
