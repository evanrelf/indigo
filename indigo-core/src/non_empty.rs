use ropey::Rope;
use std::ops::Deref;

pub struct NonEmpty<T>(T);

impl<T> Deref for NonEmpty<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub trait IntoNonEmpty {
    fn is_empty(&self) -> bool;

    fn into_non_empty(self) -> Option<NonEmpty<Self>>
    where
        Self: Sized,
    {
        if self.is_empty() {
            None
        } else {
            Some(NonEmpty(self))
        }
    }
}

impl<T> IntoNonEmpty for Vec<T> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

impl IntoNonEmpty for Rope {
    fn is_empty(&self) -> bool {
        self.len_chars() == 0
    }
}
