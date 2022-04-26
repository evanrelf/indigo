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
    fn entangle_with<Context>(self, context: &Context) -> Entangled<'_, Self> {
        Entangled {
            value: self,
            marker: PhantomData,
        }
    }
}

impl<T> Entangle for T {}

#[repr(transparent)]
pub struct Valid<'context, T>(Entangled<'context, T>);

impl<T> AsRef<T> for Valid<'_, T> {
    fn as_ref(&self) -> &T {
        &self.0.value
    }
}

impl<T> AsMut<T> for Valid<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.0.value
    }
}

impl<T> Deref for Valid<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0.value
    }
}

impl<T> DerefMut for Valid<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0.value
    }
}

// TODO: Return `Option`
pub trait Validate<Context>: Sized {
    fn is_valid(&self, context: &Context) -> bool;

    fn valid_for(self, context: &Context) -> Valid<'_, Self> {
        Valid(self.entangle_with(context))
    }

    fn valid_forever(self) -> Valid<'static, Self> {
        Valid(Entangled {
            value: self,
            marker: PhantomData,
        })
    }
}

mod test {
    #![allow(dead_code)]

    /// ```
    /// use indigo_core::valid::Validate as _;
    /// use ropey::Rope;
    /// let mut rope = Rope::from("Hello\nworld\n");
    /// let last = rope.len_chars().saturating_sub(1).valid_for(&rope);
    /// println!("{}", *last);
    /// ```
    fn test_good() {}

    /// ```compile_fail
    /// use indigo_core::valid::Validate as _;
    /// use ropey::Rope;
    /// let mut rope = Rope::from("Hello\nworld\n");
    /// let last = rope.len_chars().saturating_sub(1).valid_for(&rope);
    /// string.clear();
    /// println!("{}", *last);
    /// ```
    fn test_bad() {}
}
