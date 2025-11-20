use anyhow::anyhow;

mod t {
    pub use ratatui::crossterm::event::{
        KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers,
    };
}

mod i {
    pub use indigo_core::event::{KeyEvent, KeyEventKind};
    pub use indigo_core::key::{Key, KeyCode, KeyModifiers};
}

pub fn key_event_t2i(key_event: &t::KeyEvent) -> anyhow::Result<i::KeyEvent> {
    Ok(i::KeyEvent {
        key: i::Key {
            modifiers: key_modifiers_t2i(&key_event.modifiers)?,
            code: key_code_t2i(&key_event.code)?,
        },
        kind: key_event_kind_t2i(&key_event.kind),
    })
}

#[must_use]
pub fn key_event_i2t(key_event: &i::KeyEvent) -> t::KeyEvent {
    t::KeyEvent {
        modifiers: key_modifiers_i2t(&key_event.key.modifiers),
        code: key_code_i2t(&key_event.key.code),
        kind: key_event_kind_i2t(&key_event.kind),
        state: t::KeyEventState::NONE,
    }
}

pub fn key_modifiers_t2i(modifiers: &t::KeyModifiers) -> anyhow::Result<i::KeyModifiers> {
    modifiers
        .iter_names()
        .map(|m| match m {
            ("SHIFT", _) => Ok(i::KeyModifiers::SHIFT),
            ("CONTROL", _) => Ok(i::KeyModifiers::CONTROL),
            ("ALT", _) => Ok(i::KeyModifiers::ALT),
            (_, m) => Err(anyhow!("Unsupported crossterm key modifier: {m:?}")),
        })
        .try_fold(i::KeyModifiers::empty(), |ms, m| m.map(|m| ms | m))
}

#[must_use]
pub fn key_modifiers_i2t(modifiers: &i::KeyModifiers) -> t::KeyModifiers {
    modifiers
        .iter_names()
        .map(|m| match m {
            ("SHIFT", _) => Ok(t::KeyModifiers::SHIFT),
            ("CONTROL", _) => Ok(t::KeyModifiers::CONTROL),
            ("ALT", _) => Ok(t::KeyModifiers::ALT),
            (_, m) => Err(anyhow!("Unsupported indigo key modifier: {m:?}")),
        })
        .try_fold(t::KeyModifiers::empty(), |ms, m| m.map(|m| ms | m))
        .expect("Indigo modifiers are a subset of terminal modifiers")
}

pub fn key_code_t2i(code: &t::KeyCode) -> anyhow::Result<i::KeyCode> {
    match code {
        t::KeyCode::Backspace => Ok(i::KeyCode::Backspace),
        t::KeyCode::Delete => Ok(i::KeyCode::Delete),
        t::KeyCode::Enter => Ok(i::KeyCode::Return),
        t::KeyCode::Left => Ok(i::KeyCode::Left),
        t::KeyCode::Right => Ok(i::KeyCode::Right),
        t::KeyCode::Up => Ok(i::KeyCode::Up),
        t::KeyCode::Down => Ok(i::KeyCode::Down),
        t::KeyCode::Tab => Ok(i::KeyCode::Tab),
        t::KeyCode::Char(c) => Ok(i::KeyCode::Char(*c)),
        t::KeyCode::Esc => Ok(i::KeyCode::Escape),
        _ => Err(anyhow!("Unsupported crossterm key code: {code:?}")),
    }
}

#[must_use]
pub fn key_code_i2t(code: &i::KeyCode) -> t::KeyCode {
    match code {
        i::KeyCode::Backspace => t::KeyCode::Backspace,
        i::KeyCode::Delete => t::KeyCode::Delete,
        i::KeyCode::Return => t::KeyCode::Enter,
        i::KeyCode::Left => t::KeyCode::Left,
        i::KeyCode::Right => t::KeyCode::Right,
        i::KeyCode::Up => t::KeyCode::Up,
        i::KeyCode::Down => t::KeyCode::Down,
        i::KeyCode::Tab => t::KeyCode::Tab,
        i::KeyCode::Escape => t::KeyCode::Esc,
        i::KeyCode::Char(c) => t::KeyCode::Char(*c),
    }
}

#[must_use]
pub fn key_event_kind_t2i(kind: &t::KeyEventKind) -> i::KeyEventKind {
    match kind {
        t::KeyEventKind::Press => i::KeyEventKind::Press,
        t::KeyEventKind::Repeat => i::KeyEventKind::Repeat,
        t::KeyEventKind::Release => i::KeyEventKind::Release,
    }
}

#[must_use]
pub fn key_event_kind_i2t(kind: &i::KeyEventKind) -> t::KeyEventKind {
    match kind {
        i::KeyEventKind::Press => t::KeyEventKind::Press,
        i::KeyEventKind::Repeat => t::KeyEventKind::Repeat,
        i::KeyEventKind::Release => t::KeyEventKind::Release,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_roundtrip() {
        let i = i::KeyEvent {
            key: i::Key {
                modifiers: i::KeyModifiers::CONTROL | i::KeyModifiers::SHIFT,
                code: i::KeyCode::Char('a'),
            },
            kind: i::KeyEventKind::Press,
        };
        let c = t::KeyEvent {
            modifiers: t::KeyModifiers::CONTROL | t::KeyModifiers::SHIFT,
            code: t::KeyCode::Char('a'),
            kind: t::KeyEventKind::Press,
            state: t::KeyEventState::NONE,
        };
        assert_eq!(key_event_t2i(&c).unwrap(), i);
        assert_eq!(key_event_t2i(&key_event_i2t(&i)).unwrap(), i);
        assert_eq!(key_event_i2t(&i), c);
        assert_eq!(key_event_i2t(&key_event_t2i(&c).unwrap()), c);
    }
}
