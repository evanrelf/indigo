use crate::key as value;
use crate::Reflect;
use flagset::FlagSet;
use std::marker::PhantomData;

pub struct Key<T> {
    phantom: PhantomData<fn() -> T>,
}

impl<C> Reflect for Key<C>
where
    C: KeyCode,
{
    type Value = value::Key;

    fn reflect() -> Self::Value {
        value::Key {
            modifiers: FlagSet::default(),
            code: C::reflect(),
        }
    }
}

impl<M, C> Reflect for Key<(M, C)>
where
    M: KeyModifier,
    C: KeyCode,
{
    type Value = value::Key;

    fn reflect() -> Self::Value {
        value::Key {
            modifiers: M::reflect().into(),
            code: C::reflect(),
        }
    }
}

impl<M1, M2, C> Reflect for Key<(M1, M2, C)>
where
    M1: KeyModifier,
    M2: KeyModifier,
    C: KeyCode,
{
    type Value = value::Key;

    fn reflect() -> Self::Value {
        value::Key {
            modifiers: M1::reflect() | M2::reflect(),
            code: C::reflect(),
        }
    }
}

impl<M1, M2, M3, C> Reflect for Key<(M1, M2, M3, C)>
where
    M1: KeyModifier,
    M2: KeyModifier,
    M3: KeyModifier,
    C: KeyCode,
{
    type Value = value::Key;

    fn reflect() -> Self::Value {
        value::Key {
            modifiers: M1::reflect() | M2::reflect() | M3::reflect(),
            code: C::reflect(),
        }
    }
}

pub trait KeyModifier: Reflect<Value = value::KeyModifier> {}

pub enum Shift {}

impl Reflect for Shift {
    type Value = value::KeyModifier;

    fn reflect() -> Self::Value {
        value::KeyModifier::Shift
    }
}

impl KeyModifier for Shift {}

pub enum Control {}

impl Reflect for Control {
    type Value = value::KeyModifier;

    fn reflect() -> Self::Value {
        value::KeyModifier::Control
    }
}

impl KeyModifier for Control {}

pub enum Alt {}

impl Reflect for Alt {
    type Value = value::KeyModifier;

    fn reflect() -> Self::Value {
        value::KeyModifier::Alt
    }
}

impl KeyModifier for Alt {}

pub trait KeyCode: Reflect<Value = value::KeyCode> {}

pub enum Backspace {}

impl Reflect for Backspace {
    type Value = value::KeyCode;

    fn reflect() -> Self::Value {
        value::KeyCode::Backspace
    }
}

impl KeyCode for Backspace {}

pub enum Enter {}

impl Reflect for Enter {
    type Value = value::KeyCode;

    fn reflect() -> Self::Value {
        value::KeyCode::Enter
    }
}

impl KeyCode for Enter {}

pub enum Left {}

impl Reflect for Left {
    type Value = value::KeyCode;

    fn reflect() -> Self::Value {
        value::KeyCode::Left
    }
}

impl KeyCode for Left {}

pub enum Right {}

impl Reflect for Right {
    type Value = value::KeyCode;

    fn reflect() -> Self::Value {
        value::KeyCode::Right
    }
}

impl KeyCode for Right {}

pub enum Up {}

impl Reflect for Up {
    type Value = value::KeyCode;

    fn reflect() -> Self::Value {
        value::KeyCode::Up
    }
}

impl KeyCode for Up {}

pub enum Down {}

impl Reflect for Down {
    type Value = value::KeyCode;

    fn reflect() -> Self::Value {
        value::KeyCode::Down
    }
}

impl KeyCode for Down {}

pub enum Tab {}

impl Reflect for Tab {
    type Value = value::KeyCode;

    fn reflect() -> Self::Value {
        value::KeyCode::Tab
    }
}

impl KeyCode for Tab {}

pub enum Escape {}

impl Reflect for Escape {
    type Value = value::KeyCode;

    fn reflect() -> Self::Value {
        value::KeyCode::Escape
    }
}

impl KeyCode for Escape {}

pub enum Char<const C: char> {}

impl<const C: char> Reflect for Char<C> {
    type Value = value::KeyCode;

    fn reflect() -> Self::Value {
        value::KeyCode::Char(C)
    }
}

impl<const C: char> KeyCode for Char<C> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        // `KeyCode`s
        assert_eq!(Escape::reflect(), value::KeyCode::Escape);
        assert_eq!(Char::<'a'>::reflect(), value::KeyCode::Char('a'));

        // `KeyModifier`s
        assert_eq!(Shift::reflect(), value::KeyModifier::Shift);
        assert_eq!(Control::reflect(), value::KeyModifier::Control);
        assert_eq!(Alt::reflect(), value::KeyModifier::Alt);

        // `Key`s
        assert_eq!(
            Key::<Escape>::reflect(),
            value::Key::from(value::KeyCode::Escape),
        );
        assert_eq!(
            Key::<(Control, Char<'c'>)>::reflect(),
            value::Key::from(([value::KeyModifier::Control], value::KeyCode::Char('c'))),
        );
        assert_eq!(
            Key::<(Control, Shift, Backspace)>::reflect(),
            value::Key::from((
                [value::KeyModifier::Control, value::KeyModifier::Shift],
                value::KeyCode::Backspace
            )),
        );
    }
}
