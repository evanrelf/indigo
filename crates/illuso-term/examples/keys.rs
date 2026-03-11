use illuso_term::{
    Event, Reader, Tty, cursor,
    key::{self, Key, KeyCode, KeyModifiers, KeyboardEnhancementFlags as KKF},
    output,
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
        key::KeyboardEnhancementFlagsPush(
            KKF::DISAMBIGUATE | KKF::REPORT_EVENTS | KKF::REPORT_ALL_KEYS
        )
    )?;
    tty.flush()?;

    let mut repeat_count: u32 = 0;

    loop {
        let event = reader.read_event(&mut tty)?;
        match &event {
            Event::KeyPress(_) | Event::KeyRelease(_) => {
                repeat_count = 0;
                write!(tty, "{event:?}\n\r")?;
                tty.flush()?;
                if event == CTRL_C {
                    break;
                }
            }
            Event::KeyRepeat(_) => {
                repeat_count += 1;
                if repeat_count == 1 {
                    write!(tty, "{event:?}\n\r")?;
                } else {
                    write!(
                        tty,
                        "{}\r{}{event:?} (x{repeat_count})\n\r",
                        cursor::CursorUp(1),
                        output::CLEAR_LINE
                    )?;
                }
                tty.flush()?;
            }
            _ => {}
        }
    }

    write!(tty, "{}", key::KeyboardEnhancementFlagsPop)?;
    tty.flush()?;

    Ok(())
}
