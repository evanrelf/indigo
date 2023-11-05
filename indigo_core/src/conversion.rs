use std::fmt::Debug;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Conversion<T, E> {
    Invalid(E),
    Corrected(T),
    Valid(T),
}

impl<T, E> Conversion<T, E> {
    pub fn unwrap(self) -> T
    where
        E: Debug,
    {
        match self {
            Self::Invalid(e) => {
                panic!("called `Conversion::unwrap()` on an `Invalid` value: {e:?}")
            }
            Self::Corrected(t) | Self::Valid(t) => t,
        }
    }

    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Self::Invalid(_) => default,
            Self::Corrected(t) | Self::Valid(t) => t,
        }
    }

    pub fn unwrap_or_else<F>(self, op: F) -> T
    where
        F: FnOnce(E) -> T,
    {
        match self {
            Self::Invalid(e) => op(e),
            Self::Corrected(t) | Self::Valid(t) => t,
        }
    }

    pub fn unwrap_or_default(self) -> T
    where
        T: Default,
    {
        match self {
            Self::Invalid(_) => T::default(),
            Self::Corrected(t) | Self::Valid(t) => t,
        }
    }
}
