use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

#[repr(transparent)]
pub struct Valid<'context, T> {
    value: T,
    marker: PhantomData<&'context ()>,
}

impl<T> Valid<'_, T> {
    pub fn new<C>(value: T, _context: &C) -> Valid<'_, T> {
        Valid {
            value,
            marker: PhantomData,
        }
    }
}

impl<T> AsRef<T> for Valid<'_, T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> AsMut<T> for Valid<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> Deref for Valid<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Valid<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

pub trait ValidFor: Sized {
    fn valid_for<C>(self, context: &C) -> Valid<'_, Self> {
        Valid::new(self, context)
    }
}

impl<T> ValidFor for T {}

mod test {
    #![allow(dead_code)]

    /// ```
    /// use indigo_core::ValidFor as _;
    /// let mut string = String::from("Hello\nworld\n");
    /// let length = string.len().valid_for(&string);
    /// println!("{}", *length);
    /// ```
    fn test_good() {}

    /// ```compile_fail
    /// use indigo_core::ValidFor as _;
    /// let mut string = String::from("Hello\nworld\n");
    /// let length = string.len().valid_for(&string);
    /// string.clear();
    /// println!("{}", *length);
    /// ```
    fn test_bad() {}
}
