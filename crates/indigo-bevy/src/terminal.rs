use bevy::{
    prelude::*,
    tasks::{IoTaskPool, futures_lite::StreamExt as _},
};

#[derive(Deref, DerefMut, Resource)]
pub struct Terminal(pub ratatui::DefaultTerminal);

#[derive(Resource)]
pub struct TerminalEventReader(flume::Receiver<crossterm::event::Event>);

impl Drop for Terminal {
    fn drop(&mut self) {
        ratatui::restore();
    }
}

#[derive(Deref, DerefMut, Event)]
pub struct TerminalEvent(pub crossterm::event::Event);

#[derive(Event)]
pub enum TerminalFocusEvent {
    Gained,
    Lost,
}

#[derive(Deref, DerefMut, Event)]
pub struct TerminalKeyEvent(pub crossterm::event::KeyEvent);

#[derive(Deref, DerefMut, Event)]
pub struct TerminalMouseEvent(pub crossterm::event::MouseEvent);

#[derive(Deref, DerefMut, Event)]
pub struct TerminalPasteEvent(pub String);

#[derive(Deref, DerefMut, Event)]
pub struct TerminalResizeEvent(pub ratatui::layout::Size);

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
                if sender.send_async(event).await.is_err() {
                    break;
                }
            }
        });
        task.detach();
    }

    pub fn writer_system(
        reader: Res<TerminalEventReader>,
        mut terminal: EventWriter<TerminalEvent>,
        mut focus: EventWriter<TerminalFocusEvent>,
        mut key: EventWriter<TerminalKeyEvent>,
        mut mouse: EventWriter<TerminalMouseEvent>,
        mut paste: EventWriter<TerminalPasteEvent>,
        mut resize: EventWriter<TerminalResizeEvent>,
    ) {
        use crossterm::event::Event;
        use ratatui::layout::Size;

        for event in reader.0.try_iter() {
            terminal.write(TerminalEvent(event.clone()));
            match event {
                Event::FocusGained => {
                    focus.write(TerminalFocusEvent::Gained);
                }
                Event::FocusLost => {
                    focus.write(TerminalFocusEvent::Lost);
                }
                Event::Key(key_event) => {
                    key.write(TerminalKeyEvent(key_event));
                }
                Event::Mouse(mouse_event) => {
                    mouse.write(TerminalMouseEvent(mouse_event));
                }
                Event::Paste(text) => {
                    paste.write(TerminalPasteEvent(text));
                }
                Event::Resize(columns, rows) => {
                    resize.write(TerminalResizeEvent(Size::new(columns, rows)));
                }
            }
        }
    }
}

impl Plugin for TuiPlugin {
    fn build(&self, app: &mut App) {
        let terminal = ratatui::init();
        app.insert_resource(Terminal(terminal))
            .add_event::<TerminalEvent>()
            .add_event::<TerminalFocusEvent>()
            .add_event::<TerminalKeyEvent>()
            .add_event::<TerminalMouseEvent>()
            .add_event::<TerminalPasteEvent>()
            .add_event::<TerminalResizeEvent>()
            .add_systems(Startup, Self::reader_system)
            .add_systems(PreUpdate, Self::writer_system);
    }
}
