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

    pub fn get(self) -> T {
        self.value
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

mod test {
    #![allow(dead_code)]

    /// ```
    /// use indigo_core::Valid;
    /// let mut string = String::from("Hello\nworld\n");
    /// let length = Valid::new(string.len(), &string);
    /// println!("{}", length.get());
    /// ```
    fn test_good() {}

    /// ```compile_fail
    /// use indigo_core::Valid;
    /// let mut string = String::from("Hello\nworld\n");
    /// let length = Valid::new(string.len(), &string);
    /// string.clear();
    /// println!("{}", length.get());
    /// ```
    fn test_bad() {}
}
