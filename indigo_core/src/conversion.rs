use std::fmt::Debug;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Conversion<T, E> {
    Invalid(E),
    Corrected(T),
    Valid(T),
}

impl<T, E> Conversion<T, E> {
    pub fn map<U, F>(self, op: F) -> Conversion<U, E>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Invalid(e) => Conversion::Invalid(e),
            Self::Corrected(t) => Conversion::Corrected(op(t)),
            Self::Valid(t) => Conversion::Valid(op(t)),
        }
    }

    pub fn map_invalid<F, O>(self, op: O) -> Conversion<T, F>
    where
        O: FnOnce(E) -> F,
    {
        match self {
            Self::Invalid(e) => Conversion::Invalid(op(e)),
            Self::Corrected(t) => Conversion::Corrected(t),
            Self::Valid(t) => Conversion::Valid(t),
        }
    }

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
