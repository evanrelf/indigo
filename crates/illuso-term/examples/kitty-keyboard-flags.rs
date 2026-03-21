#![allow(clippy::uninlined_format_args)]
#![allow(clippy::wildcard_imports)]

use illuso_term::{KeyboardEnhancementFlags as KEF, *};
use std::io::{self, Write as _};

fn main() -> io::Result<()> {
    let mut tty = Tty::init()?;
    let mut reader = Reader::new();

    ////////////////////////////////////////////////////////////////////////////////////////////////

    write!(tty, "querying kitty keyboard flags...\n\r")?;
    write!(tty, "{}", KEYBOARD_ENHANCEMENT_FLAGS_QUERY)?;
    tty.flush()?;
    let Event::KeyboardEnhancementFlags(flags) = reader.read_event(&mut tty)? else {
        drop(tty);
        panic!("expected kitty keyboard flags, actually read a different event");
    };
    write!(tty, "{flags:?}\n\n\r")?;
    tty.flush()?;

    ////////////////////////////////////////////////////////////////////////////////////////////////

    write!(tty, "pushing kitty keyboard flags...\n\r")?;
    write!(
        tty,
        "{}",
        KeyboardEnhancementFlagsPush(KEF::DISAMBIGUATE | KEF::REPORT_EVENTS)
    )?;
    tty.flush()?;

    ////////////////////////////////////////////////////////////////////////////////////////////////

    write!(tty, "querying kitty keyboard flags...\n\r")?;
    write!(tty, "{}", KEYBOARD_ENHANCEMENT_FLAGS_QUERY)?;
    tty.flush()?;
    let Event::KeyboardEnhancementFlags(flags) = reader.read_event(&mut tty)? else {
        drop(tty);
        panic!("expected kitty keyboard flags, actually read a different event");
    };
    write!(tty, "{flags:?}\n\n\r")?;
    tty.flush()?;

    ////////////////////////////////////////////////////////////////////////////////////////////////

    write!(tty, "popping kitty keyboard flags...\n\r")?;
    write!(tty, "{}", KeyboardEnhancementFlagsPop)?;
    tty.flush()?;

    ////////////////////////////////////////////////////////////////////////////////////////////////

    write!(tty, "querying kitty keyboard flags...\n\r")?;
    write!(tty, "{}", KEYBOARD_ENHANCEMENT_FLAGS_QUERY)?;
    tty.flush()?;
    let Event::KeyboardEnhancementFlags(flags) = reader.read_event(&mut tty)? else {
        drop(tty);
        panic!("expected kitty keyboard flags, actually read a different event");
    };
    write!(tty, "{flags:?}\n\r")?;
    tty.flush()?;

    ////////////////////////////////////////////////////////////////////////////////////////////////

    Ok(())
}
