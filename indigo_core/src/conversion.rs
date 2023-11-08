use std::fmt::Debug;

#[must_use]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Conversion<T, E> {
    Invalid(E),
    Corrected(T),
    Valid(T),
}

impl<T, E> Conversion<T, E> {
    pub fn is_invalid(&self) -> bool {
        matches!(self, Self::Invalid(_))
    }

    pub fn is_corrected(&self) -> bool {
        matches!(self, Self::Corrected(_))
    }

    pub fn is_valid(&self) -> bool {
        matches!(self, Self::Valid(_))
    }

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

impl<T, E> From<Conversion<T, E>> for Option<T> {
    fn from(conversion: Conversion<T, E>) -> Self {
        match conversion {
            Conversion::Invalid(_) => Self::None,
            Conversion::Corrected(t) | Conversion::Valid(t) => Self::Some(t),
        }
    }
}

impl<T, E> From<Conversion<T, E>> for Result<T, E> {
    fn from(conversion: Conversion<T, E>) -> Self {
        match conversion {
            Conversion::Invalid(e) => Self::Err(e),
            Conversion::Corrected(t) | Conversion::Valid(t) => Self::Ok(t),
        }
    }
}
