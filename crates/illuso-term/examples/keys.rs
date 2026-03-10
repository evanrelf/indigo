use illuso_term::{
    Event, Reader, Tty,
    escape::{self, KittyKeyboardFlags as KKF},
    event::{Key, KeyCode, KeyModifiers},
};
use std::io::{self, Write as _};

const CTRL_C: Event = Event::KeyPress(Key {
    code: KeyCode::Char('c'),
    modifiers: KeyModifiers::CTRL,
});

fn main() -> io::Result<()> {
    let mut tty = Tty::init()?;
    let mut reader = Reader::new();

    write!(
        tty,
        "{}",
        // NOTE: All these flags must be enabled to get full keyboard functionality, since the
        // raw/legacy keys are intentionally not supported.
        escape::KittyKeyboardFlagsPush(
            KKF::DISAMBIGUATE | KKF::REPORT_EVENTS | KKF::REPORT_ALL_KEYS
        )
    )?;
    tty.flush()?;

    loop {
        let event = reader.read_event(&mut tty)?;
        match &event {
            Event::KeyPress(_) | Event::KeyRepeat(_) | Event::KeyRelease(_) => {
                write!(tty, "{event:?}\n\r")?;
                tty.flush()?;
                if event == CTRL_C {
                    break;
                }
            }
            _ => {}
        }
    }

    write!(tty, "{}", escape::KittyKeyboardFlagsPop)?;
    tty.flush()?;

    Ok(())
}
