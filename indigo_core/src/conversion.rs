#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Conversion<T> {
    Invalid,
    Corrected(T),
    Valid(T),
}

impl<T> Conversion<T> {
    pub fn unwrap(self) -> T {
        match self {
            Self::Invalid => panic!("called `Conversion::unwrap()` on an `Invalid` value"),
            Self::Corrected(x) | Self::Valid(x) => x,
        }
    }

    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Self::Invalid => default,
            Self::Corrected(x) | Self::Valid(x) => x,
        }
    }

    pub fn unwrap_or_else<F>(self, op: F) -> T
    where
        F: FnOnce() -> T,
    {
        match self {
            Self::Invalid => op(),
            Self::Corrected(x) | Self::Valid(x) => x,
        }
    }

    pub fn unwrap_or_default(self) -> T
    where
        T: Default,
    {
        match self {
            Self::Invalid => T::default(),
            Self::Corrected(x) | Self::Valid(x) => x,
        }
    }
}
