use crate::park::Unparker;
use bevy::{
    prelude::*,
    tasks::{IoTaskPool, futures_lite::StreamExt as _},
};
use crossterm::{
    event::{
        DisableBracketedPaste, EnableBracketedPaste, EventStream, KeyboardEnhancementFlags as KEF,
        PopKeyboardEnhancementFlags, PushKeyboardEnhancementFlags,
    },
    execute,
};
use ratatui::layout::Size;
use std::io::stdout;

#[derive(Deref, DerefMut, Resource)]
pub struct Terminal(pub ratatui::DefaultTerminal);

impl Drop for Terminal {
    fn drop(&mut self) {
        TuiPlugin::restore();
    }
}

#[derive(Resource)]
pub struct EventReader(flume::Receiver<crossterm::event::Event>);

#[derive(Deref, DerefMut, Event)]
pub struct Event(pub crossterm::event::Event);

#[derive(Event)]
pub enum Focus {
    Gained,
    Lost,
}

#[derive(Deref, DerefMut, Event)]
pub struct Key(pub crossterm::event::KeyEvent);

#[derive(Deref, DerefMut, Event)]
pub struct Mouse(pub crossterm::event::MouseEvent);

#[derive(Deref, DerefMut, Event)]
pub struct Paste(pub String);

#[derive(Deref, DerefMut, Event)]
pub struct Resize(pub ratatui::layout::Size);

pub struct TuiPlugin;

impl TuiPlugin {
    fn init() -> Terminal {
        let terminal = ratatui::init();

        execute!(
            stdout(),
            // TODO: Disabled to avoid event spam until I'm ready to use this
            // EnableMouseCapture,
            EnableBracketedPaste,
            PushKeyboardEnhancementFlags(KEF::DISAMBIGUATE_ESCAPE_CODES),
        )
        .unwrap();

        Terminal(terminal)
    }

    fn restore() {
        ratatui::restore();

        let _ = execute!(
            stdout(),
            // TODO: Disabled to avoid event spam until I'm ready to use this
            // DisableMouseCapture,
            DisableBracketedPaste,
            PopKeyboardEnhancementFlags,
        );
    }

    pub fn reader_system(mut commands: Commands, unparker: Option<Res<Unparker>>) {
        let (sender, receiver) = flume::bounded(32);
        commands.insert_resource(EventReader(receiver));

        let unparker = unparker.map(|u| u.clone());

        let task_pool = IoTaskPool::get();
        let task = task_pool.spawn(async move {
            let mut event_stream = EventStream::new();
            while let Some(event) = event_stream.next().await.map(|result| result.unwrap()) {
                if sender.send_async(event).await.is_err() {
                    break;
                }
                if let Some(ref unparker) = unparker {
                    unparker.unpark();
                }
            }
        });
        task.detach();
    }

    pub fn writer_system(
        reader: Res<EventReader>,
        mut terminal: EventWriter<Event>,
        mut focus: EventWriter<Focus>,
        mut key: EventWriter<Key>,
        mut mouse: EventWriter<Mouse>,
        mut paste: EventWriter<Paste>,
        mut resize: EventWriter<Resize>,
    ) {
        use crossterm::event::Event as TEvent;

        for event in reader.0.try_iter() {
            terminal.write(Event(event.clone()));
            match event {
                TEvent::FocusGained => _ = focus.write(Focus::Gained),
                TEvent::FocusLost => _ = focus.write(Focus::Lost),
                TEvent::Key(key_event) => _ = key.write(Key(key_event)),
                TEvent::Mouse(mouse_event) => _ = mouse.write(Mouse(mouse_event)),
                TEvent::Paste(text) => _ = paste.write(Paste(text)),
                TEvent::Resize(columns, rows) => _ = resize.write(Resize(Size::new(columns, rows))),
            }
        }
    }
}

impl Plugin for TuiPlugin {
    fn build(&self, app: &mut App) {
        let terminal = Self::init();
        app.insert_resource(terminal)
            .add_event::<Event>()
            .add_event::<Focus>()
            .add_event::<Key>()
            .add_event::<Mouse>()
            .add_event::<Paste>()
            .add_event::<Resize>()
            .add_systems(Startup, Self::reader_system)
            .add_systems(PreUpdate, Self::writer_system);
    }
}
