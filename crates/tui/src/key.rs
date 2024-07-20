use anyhow::anyhow;
use flagset::FlagSet;

mod c {
    pub use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};
}

mod i {
    pub use indigo_core::{Key, KeyCode, KeyModifier};
}

pub fn key_c2i(key: c::KeyEvent) -> anyhow::Result<i::Key> {
    Ok(i::Key {
        modifiers: key_modifiers_c2i(key.modifiers)?,
        code: key_code_c2i(key.code)?,
    })
}

pub fn key_i2c(key: i::Key) -> c::KeyEvent {
    c::KeyEvent {
        modifiers: key_modifiers_i2c(key.modifiers),
        code: key_code_i2c(key.code),
        kind: c::KeyEventKind::Press,
        state: c::KeyEventState::NONE,
    }
}

pub fn key_modifiers_c2i(modifiers: c::KeyModifiers) -> anyhow::Result<FlagSet<i::KeyModifier>> {
    modifiers
        .iter_names()
        .map(|m| match m {
            ("SHIFT", _) => Ok(i::KeyModifier::Shift),
            ("CONTROL", _) => Ok(i::KeyModifier::Control),
            ("ALT", _) => Ok(i::KeyModifier::Alt),
            (_, m) => Err(anyhow!("Unsupported crossterm key modifier: {m:?}")),
        })
        .try_fold(FlagSet::default(), |x, y| y.map(|y| x | y))
}

pub fn key_modifiers_i2c(modifiers: FlagSet<i::KeyModifier>) -> c::KeyModifiers {
    modifiers
        .into_iter()
        .map(|m| match m {
            i::KeyModifier::Shift => c::KeyModifiers::SHIFT,
            i::KeyModifier::Control => c::KeyModifiers::CONTROL,
            i::KeyModifier::Alt => c::KeyModifiers::ALT,
        })
        .fold(c::KeyModifiers::NONE, |x, y| x | y)
}

pub fn key_code_c2i(code: c::KeyCode) -> anyhow::Result<i::KeyCode> {
    match code {
        c::KeyCode::Backspace => Ok(i::KeyCode::Backspace),
        c::KeyCode::Enter => Ok(i::KeyCode::Enter),
        c::KeyCode::Left => Ok(i::KeyCode::Left),
        c::KeyCode::Right => Ok(i::KeyCode::Right),
        c::KeyCode::Up => Ok(i::KeyCode::Up),
        c::KeyCode::Down => Ok(i::KeyCode::Down),
        c::KeyCode::Tab => Ok(i::KeyCode::Tab),
        c::KeyCode::Char(c) => Ok(i::KeyCode::Char(c)),
        c::KeyCode::Esc => Ok(i::KeyCode::Escape),
        _ => Err(anyhow!("Unsupported crossterm key code: {code:?}")),
    }
}

pub fn key_code_i2c(code: i::KeyCode) -> c::KeyCode {
    match code {
        i::KeyCode::Backspace => c::KeyCode::Backspace,
        i::KeyCode::Enter => c::KeyCode::Enter,
        i::KeyCode::Left => c::KeyCode::Left,
        i::KeyCode::Right => c::KeyCode::Right,
        i::KeyCode::Up => c::KeyCode::Up,
        i::KeyCode::Down => c::KeyCode::Down,
        i::KeyCode::Tab => c::KeyCode::Tab,
        i::KeyCode::Escape => c::KeyCode::Esc,
        i::KeyCode::Char(c) => c::KeyCode::Char(c),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip() {
        let i = i::Key::from(([i::KeyModifier::Control, i::KeyModifier::Shift], 'a'));
        let c = c::KeyEvent {
            modifiers: c::KeyModifiers::CONTROL | c::KeyModifiers::SHIFT,
            code: c::KeyCode::Char('a'),
            kind: c::KeyEventKind::Press,
            state: c::KeyEventState::NONE,
        };
        assert_eq!(key_c2i(c).unwrap(), i);
        assert_eq!(key_c2i(key_i2c(i)).unwrap(), i);
        assert_eq!(key_i2c(i), c);
        assert_eq!(key_c2i(c).map(key_i2c).unwrap(), c);
    }
}
