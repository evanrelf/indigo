use anyhow::anyhow;
use flagset::FlagSet;

pub type IndigoKey = indigo_core::key::Key;
pub type IndigoKeyCode = indigo_core::key::KeyCode;
pub type IndigoKeyModifiers = indigo_core::key::KeyModifiers;

pub type TerminalKey = ratatui::crossterm::event::KeyEvent;
pub type TerminalKeyCode = ratatui::crossterm::event::KeyCode;
pub type TerminalKeyModifiers = ratatui::crossterm::event::KeyModifiers;

mod t {
    pub use ratatui::crossterm::event::{KeyEventKind, KeyEventState};
}

mod i {
    pub use indigo_core::key::KeyModifier;
}

pub fn key_t2i(key: TerminalKey) -> anyhow::Result<IndigoKey> {
    Ok(IndigoKey {
        modifiers: key_modifiers_t2i(key.modifiers)?,
        code: key_code_t2i(key.code)?,
    })
}

#[must_use]
pub fn key_i2t(key: IndigoKey) -> TerminalKey {
    TerminalKey {
        modifiers: key_modifiers_i2t(key.modifiers),
        code: key_code_i2t(key.code),
        kind: t::KeyEventKind::Press,
        state: t::KeyEventState::NONE,
    }
}

pub fn key_modifiers_t2i(modifiers: TerminalKeyModifiers) -> anyhow::Result<IndigoKeyModifiers> {
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

#[must_use]
pub fn key_modifiers_i2t(modifiers: IndigoKeyModifiers) -> TerminalKeyModifiers {
    modifiers
        .into_iter()
        .map(|m| match m {
            i::KeyModifier::Shift => TerminalKeyModifiers::SHIFT,
            i::KeyModifier::Control => TerminalKeyModifiers::CONTROL,
            i::KeyModifier::Alt => TerminalKeyModifiers::ALT,
        })
        .fold(TerminalKeyModifiers::NONE, |x, y| x | y)
}

pub fn key_code_t2i(code: TerminalKeyCode) -> anyhow::Result<IndigoKeyCode> {
    match code {
        TerminalKeyCode::Backspace => Ok(IndigoKeyCode::Backspace),
        TerminalKeyCode::Delete => Ok(IndigoKeyCode::Delete),
        TerminalKeyCode::Enter => Ok(IndigoKeyCode::Return),
        TerminalKeyCode::Left => Ok(IndigoKeyCode::Left),
        TerminalKeyCode::Right => Ok(IndigoKeyCode::Right),
        TerminalKeyCode::Up => Ok(IndigoKeyCode::Up),
        TerminalKeyCode::Down => Ok(IndigoKeyCode::Down),
        TerminalKeyCode::Tab => Ok(IndigoKeyCode::Tab),
        TerminalKeyCode::Char(c) => Ok(IndigoKeyCode::Char(c)),
        TerminalKeyCode::Esc => Ok(IndigoKeyCode::Escape),
        _ => Err(anyhow!("Unsupported crossterm key code: {code:?}")),
    }
}

#[must_use]
pub fn key_code_i2t(code: IndigoKeyCode) -> TerminalKeyCode {
    match code {
        IndigoKeyCode::Backspace => TerminalKeyCode::Backspace,
        IndigoKeyCode::Delete => TerminalKeyCode::Delete,
        IndigoKeyCode::Return => TerminalKeyCode::Enter,
        IndigoKeyCode::Left => TerminalKeyCode::Left,
        IndigoKeyCode::Right => TerminalKeyCode::Right,
        IndigoKeyCode::Up => TerminalKeyCode::Up,
        IndigoKeyCode::Down => TerminalKeyCode::Down,
        IndigoKeyCode::Tab => TerminalKeyCode::Tab,
        IndigoKeyCode::Escape => TerminalKeyCode::Esc,
        IndigoKeyCode::Char(c) => TerminalKeyCode::Char(c),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip() {
        let i = IndigoKey::from(([i::KeyModifier::Control, i::KeyModifier::Shift], 'a'));
        let c = TerminalKey {
            modifiers: TerminalKeyModifiers::CONTROL | TerminalKeyModifiers::SHIFT,
            code: TerminalKeyCode::Char('a'),
            kind: t::KeyEventKind::Press,
            state: t::KeyEventState::NONE,
        };
        assert_eq!(key_t2i(c).unwrap(), i);
        assert_eq!(key_t2i(key_i2t(i)).unwrap(), i);
        assert_eq!(key_i2t(i), c);
        assert_eq!(key_t2i(c).map(key_i2t).unwrap(), c);
    }
}
