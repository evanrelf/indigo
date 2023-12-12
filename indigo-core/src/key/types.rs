use crate::key::values;
use crate::Reflect;
use flagset::FlagSet;
use std::convert::Infallible;
use std::marker::PhantomData;

pub struct Key<T> {
    never: Infallible,
    phantom: PhantomData<fn() -> T>,
}

#[must_use]
pub fn key<K>() -> <Key<K> as Reflect>::Value
where
    Key<K>: Reflect<Value = values::Key>,
{
    Key::<K>::reflect()
}

impl<C> Reflect for Key<C>
where
    C: KeyCode,
{
    type Value = values::Key;

    fn reflect() -> Self::Value {
        values::Key {
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
    type Value = values::Key;

    fn reflect() -> Self::Value {
        values::Key {
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
    type Value = values::Key;

    fn reflect() -> Self::Value {
        values::Key {
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
    type Value = values::Key;

    fn reflect() -> Self::Value {
        values::Key {
            modifiers: M1::reflect() | M2::reflect() | M3::reflect(),
            code: C::reflect(),
        }
    }
}

pub trait KeyModifier: Reflect<Value = values::KeyModifier> {}

pub enum Shift {}

impl Reflect for Shift {
    type Value = values::KeyModifier;

    fn reflect() -> Self::Value {
        values::KeyModifier::Shift
    }
}

impl KeyModifier for Shift {}

pub enum Control {}

impl Reflect for Control {
    type Value = values::KeyModifier;

    fn reflect() -> Self::Value {
        values::KeyModifier::Control
    }
}

impl KeyModifier for Control {}

pub enum Alt {}

impl Reflect for Alt {
    type Value = values::KeyModifier;

    fn reflect() -> Self::Value {
        values::KeyModifier::Alt
    }
}

impl KeyModifier for Alt {}

pub trait KeyCode: Reflect<Value = values::KeyCode> {}

pub enum Backspace {}

impl Reflect for Backspace {
    type Value = values::KeyCode;

    fn reflect() -> Self::Value {
        values::KeyCode::Backspace
    }
}

impl KeyCode for Backspace {}

pub enum Enter {}

impl Reflect for Enter {
    type Value = values::KeyCode;

    fn reflect() -> Self::Value {
        values::KeyCode::Enter
    }
}

impl KeyCode for Enter {}

pub enum Left {}

impl Reflect for Left {
    type Value = values::KeyCode;

    fn reflect() -> Self::Value {
        values::KeyCode::Left
    }
}

impl KeyCode for Left {}

pub enum Right {}

impl Reflect for Right {
    type Value = values::KeyCode;

    fn reflect() -> Self::Value {
        values::KeyCode::Right
    }
}

impl KeyCode for Right {}

pub enum Up {}

impl Reflect for Up {
    type Value = values::KeyCode;

    fn reflect() -> Self::Value {
        values::KeyCode::Up
    }
}

impl KeyCode for Up {}

pub enum Down {}

impl Reflect for Down {
    type Value = values::KeyCode;

    fn reflect() -> Self::Value {
        values::KeyCode::Down
    }
}

impl KeyCode for Down {}

pub enum Tab {}

impl Reflect for Tab {
    type Value = values::KeyCode;

    fn reflect() -> Self::Value {
        values::KeyCode::Tab
    }
}

impl KeyCode for Tab {}

pub enum Escape {}

impl Reflect for Escape {
    type Value = values::KeyCode;

    fn reflect() -> Self::Value {
        values::KeyCode::Escape
    }
}

impl KeyCode for Escape {}

pub enum Char<const C: char> {}

impl<const C: char> Reflect for Char<C> {
    type Value = values::KeyCode;

    fn reflect() -> Self::Value {
        values::KeyCode::Char(C)
    }
}

impl<const C: char> KeyCode for Char<C> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        // `KeyCode`s
        assert_eq!(Escape::reflect(), values::KeyCode::Escape);
        assert_eq!(Char::<'a'>::reflect(), values::KeyCode::Char('a'));

        // `KeyModifier`s
        assert_eq!(Shift::reflect(), values::KeyModifier::Shift);
        assert_eq!(Control::reflect(), values::KeyModifier::Control);
        assert_eq!(Alt::reflect(), values::KeyModifier::Alt);

        // `Key`s
        assert_eq!(key::<Escape>(), values::Key::from(values::KeyCode::Escape));
        assert_eq!(
            key::<(Control, Char<'c'>)>(),
            values::Key::from(([values::KeyModifier::Control], values::KeyCode::Char('c'))),
        );
        assert_eq!(
            key::<(Control, Shift, Backspace)>(),
            values::Key::from((
                [values::KeyModifier::Control, values::KeyModifier::Shift],
                values::KeyCode::Backspace
            )),
        );
    }
}
