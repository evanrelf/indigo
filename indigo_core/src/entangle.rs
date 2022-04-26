use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

#[repr(transparent)]
pub struct Entangled<'context, T> {
    value: T,
    marker: PhantomData<&'context ()>,
}

impl<T> AsRef<T> for Entangled<'_, T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> AsMut<T> for Entangled<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> Deref for Entangled<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Entangled<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

pub trait Entangle: Sized {
    fn entangle<C>(self, _context: &C) -> Entangled<'_, Self> {
        Entangled {
            value: self,
            marker: PhantomData,
        }
    }

    fn entangle_lifetime<'a>(self) -> Entangled<'a, Self> {
        Entangled {
            value: self,
            marker: PhantomData,
        }
    }
}

impl<T> Entangle for T {}

mod test {
    #![allow(dead_code)]

    /// ```
    /// use indigo_core::entangle::Entangle as _;
    /// use ropey::Rope;
    /// let mut rope = Rope::from("Hello\nworld\n");
    /// let last = rope.len_chars().saturating_sub(1).entangle(&rope);
    /// println!("{}", *last);
    /// ```
    fn test_good() {}

    /// ```compile_fail
    /// use indigo_core::entangle::Entangle as _;
    /// use ropey::Rope;
    /// let mut rope = Rope::from("Hello\nworld\n");
    /// let last = rope.len_chars().saturating_sub(1).entangle(&rope);
    /// string.clear();
    /// println!("{}", *last);
    /// ```
    fn test_bad() {}
}
