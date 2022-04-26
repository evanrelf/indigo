use crate::entangle::{Entangle, Entangled};
use std::ops::{Deref, DerefMut};

#[repr(transparent)]
pub struct Valid<'context, T>(Entangled<'context, T>);

impl<T> AsRef<T> for Valid<'_, T> {
    fn as_ref(&self) -> &T {
        &*self.0
    }
}

impl<T> AsMut<T> for Valid<'_, T> {
    fn as_mut(&mut self) -> &mut T {
        &mut *self.0
    }
}

impl<T> Deref for Valid<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T> DerefMut for Valid<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}

// TODO: Return `Option`
pub trait Validate<C>: Sized {
    fn is_valid(&self, context: &C) -> bool;

    fn valid_for(self, context: &C) -> Valid<'_, Self> {
        Valid(self.entangle(context))
    }

    fn valid_forever(self) -> Valid<'static, Self> {
        Valid(self.entangle_lifetime())
    }
}
