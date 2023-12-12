use crate::key::values as v;
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
    Key<K>: Reflect<Value = v::Key>,
{
    Key::<K>::reflect()
}

impl<C> Reflect for Key<C>
where
    C: KeyCode,
{
    type Value = v::Key;

    fn reflect() -> Self::Value {
        v::Key {
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
    type Value = v::Key;

    fn reflect() -> Self::Value {
        v::Key {
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
    type Value = v::Key;

    fn reflect() -> Self::Value {
        v::Key {
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
    type Value = v::Key;

    fn reflect() -> Self::Value {
        v::Key {
            modifiers: M1::reflect() | M2::reflect() | M3::reflect(),
            code: C::reflect(),
        }
    }
}

pub trait KeyModifier: Reflect<Value = v::KeyModifier> {}

pub enum Shift {}

impl Reflect for Shift {
    type Value = v::KeyModifier;

    fn reflect() -> Self::Value {
        v::KeyModifier::Shift
    }
}

impl KeyModifier for Shift {}

pub enum Control {}

impl Reflect for Control {
    type Value = v::KeyModifier;

    fn reflect() -> Self::Value {
        v::KeyModifier::Control
    }
}

impl KeyModifier for Control {}

pub enum Alt {}

impl Reflect for Alt {
    type Value = v::KeyModifier;

    fn reflect() -> Self::Value {
        v::KeyModifier::Alt
    }
}

impl KeyModifier for Alt {}

pub trait KeyCode: Reflect<Value = v::KeyCode> {}

pub enum Backspace {}

impl Reflect for Backspace {
    type Value = v::KeyCode;

    fn reflect() -> Self::Value {
        v::KeyCode::Backspace
    }
}

impl KeyCode for Backspace {}

pub enum Enter {}

impl Reflect for Enter {
    type Value = v::KeyCode;

    fn reflect() -> Self::Value {
        v::KeyCode::Enter
    }
}

impl KeyCode for Enter {}

pub enum Left {}

impl Reflect for Left {
    type Value = v::KeyCode;

    fn reflect() -> Self::Value {
        v::KeyCode::Left
    }
}

impl KeyCode for Left {}

pub enum Right {}

impl Reflect for Right {
    type Value = v::KeyCode;

    fn reflect() -> Self::Value {
        v::KeyCode::Right
    }
}

impl KeyCode for Right {}

pub enum Up {}

impl Reflect for Up {
    type Value = v::KeyCode;

    fn reflect() -> Self::Value {
        v::KeyCode::Up
    }
}

impl KeyCode for Up {}

pub enum Down {}

impl Reflect for Down {
    type Value = v::KeyCode;

    fn reflect() -> Self::Value {
        v::KeyCode::Down
    }
}

impl KeyCode for Down {}

pub enum Tab {}

impl Reflect for Tab {
    type Value = v::KeyCode;

    fn reflect() -> Self::Value {
        v::KeyCode::Tab
    }
}

impl KeyCode for Tab {}

pub enum Escape {}

impl Reflect for Escape {
    type Value = v::KeyCode;

    fn reflect() -> Self::Value {
        v::KeyCode::Escape
    }
}

impl KeyCode for Escape {}

pub enum Char<const C: char> {}

impl<const C: char> Reflect for Char<C> {
    type Value = v::KeyCode;

    fn reflect() -> Self::Value {
        v::KeyCode::Char(C)
    }
}

impl<const C: char> KeyCode for Char<C> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        // `KeyCode`s
        assert_eq!(Escape::reflect(), v::KeyCode::Escape);
        assert_eq!(Char::<'a'>::reflect(), v::KeyCode::Char('a'));

        // `KeyModifier`s
        assert_eq!(Shift::reflect(), v::KeyModifier::Shift);
        assert_eq!(Control::reflect(), v::KeyModifier::Control);
        assert_eq!(Alt::reflect(), v::KeyModifier::Alt);

        // `Key`s
        assert_eq!(key::<Escape>(), v::Key::from(v::KeyCode::Escape));
        assert_eq!(
            key::<(Control, Char<'c'>)>(),
            v::Key::from(([v::KeyModifier::Control], v::KeyCode::Char('c'))),
        );
        assert_eq!(
            key::<(Control, Shift, Backspace)>(),
            v::Key::from((
                [v::KeyModifier::Control, v::KeyModifier::Shift],
                v::KeyCode::Backspace
            )),
        );
    }
}
