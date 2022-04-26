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

pub trait Validate<C>: Sized {
    fn is_valid(&self, context: Option<&C>) -> bool;

    fn valid_for(self, context: &C) -> Option<Valid<'_, Self>> {
        if self.is_valid(Some(context)) {
            Some(Valid(self.entangle(context)))
        } else {
            None
        }
    }

    fn valid_forever(self) -> Option<Valid<'static, Self>> {
        if self.is_valid(None) {
            Some(Valid(self.entangle_lifetime()))
        } else {
            None
        }
    }
}
