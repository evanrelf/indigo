use flagset::FlagSet;

mod c {
    pub use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};
}

mod i {
    pub use indigo_core::{Key, KeyCode, KeyModifier};
}

pub fn key_c2i(key: c::KeyEvent) -> Option<i::Key> {
    Some(i::Key {
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

pub fn key_modifiers_c2i(modifiers: c::KeyModifiers) -> Option<FlagSet<i::KeyModifier>> {
    modifiers
        .iter_names()
        .map(|m| match m {
            ("SHIFT", _) => Some(i::KeyModifier::Shift),
            ("CONTROL", _) => Some(i::KeyModifier::Ctrl),
            ("ALT", _) => Some(i::KeyModifier::Alt),
            _ => None,
        })
        .try_fold(FlagSet::default(), |x, y| y.map(|y| x | y))
}

pub fn key_modifiers_i2c(modifiers: FlagSet<i::KeyModifier>) -> c::KeyModifiers {
    modifiers
        .into_iter()
        .map(|m| match m {
            i::KeyModifier::Shift => c::KeyModifiers::SHIFT,
            i::KeyModifier::Ctrl => c::KeyModifiers::CONTROL,
            i::KeyModifier::Alt => c::KeyModifiers::ALT,
        })
        .fold(c::KeyModifiers::NONE, |x, y| x | y)
}

pub fn key_code_c2i(code: c::KeyCode) -> Option<i::KeyCode> {
    match code {
        c::KeyCode::Backspace => Some(i::KeyCode::Backspace),
        c::KeyCode::Enter => Some(i::KeyCode::Enter),
        c::KeyCode::Left => Some(i::KeyCode::Left),
        c::KeyCode::Right => Some(i::KeyCode::Right),
        c::KeyCode::Up => Some(i::KeyCode::Up),
        c::KeyCode::Down => Some(i::KeyCode::Down),
        c::KeyCode::Tab => Some(i::KeyCode::Tab),
        c::KeyCode::Char(c) => Some(i::KeyCode::Char(c)),
        c::KeyCode::Esc => Some(i::KeyCode::Escape),
        _ => None,
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
        let i = i::Key::from(([i::KeyModifier::Ctrl, i::KeyModifier::Shift], 'a'));
        let c = c::KeyEvent {
            modifiers: c::KeyModifiers::CONTROL | c::KeyModifiers::SHIFT,
            code: c::KeyCode::Char('a'),
            kind: c::KeyEventKind::Press,
            state: c::KeyEventState::NONE,
        };
        assert_eq!(Some(i), key_c2i(c));
        assert_eq!(Some(i), key_c2i(key_i2c(i)));
        assert_eq!(c, key_i2c(i));
        assert_eq!(Some(c), key_c2i(c).map(key_i2c));
    }
}
