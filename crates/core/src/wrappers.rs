/*!

## Example

```rust
use indigo_core::wrappers::*;
use ropey::Rope;

struct CursorState {
    char_offset: usize,
}

// Use the weakest trait bound `Wrap` when defining the type to perform wrapping.
struct CursorView<'a, S: Wrap, T: Wrap = S> {
    text: T::Wrap<'a, Rope>,
    state: S::Wrap<'a, CursorState>,
}

// Use the stronger trait bound `WrapRef` if you need to read values.
impl<W: WrapRef> CursorView<'_, W> {
    fn char_offset(&self) -> usize {
        self.state.char_offset
    }
}

// Use the strongest trait bound `WrapMut` if you need to mutate values.
impl<W: WrapMut> CursorView<'_, W> {
    fn insert(&mut self, text: &str) {
        self.text.insert(self.state.char_offset, text);
    }
}

// Has `char_offset` method.
pub type Cursor<'a> = CursorView<'a, Immutable>;

// Has `char_offset` and `insert` methods.
pub type CursorMut<'a> = CursorView<'a, Mutable>;
```

*/

use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
};

/// Generic type wrappers. A limited form of higher-kinded types in Rust.
///
/// This is a generalized version of `WrapRef` that doesn't require `Deref`, so you can use wrappers
/// that may not have the type they're wrapping (e.g. `PhantomData`, `Option`, etc).
///
/// If your wrapper type implements `Deref`, you should implement the `WrapRef` trait instead, and
/// you'll get a `Wrap` impl for free.
pub trait Wrap {
    type Wrap<'a, T: ?Sized + 'a>;
}

/// Phantom type (`PhantomData<T>`)
pub struct Phantom;

/// Turn `T` into `PhantomData<T>`
impl Wrap for Phantom {
    type Wrap<'a, T: ?Sized + 'a> = PhantomData<T>;
}

/// Anything implementing `WrapRef` trivially implements `Wrap`.
impl<R: WrapRef> Wrap for R {
    type Wrap<'a, T: ?Sized + 'a> = R::WrapRef<'a, T>;
}

/// Generic immutable/shared reference type wrappers. A limited form of higher-kinded types in Rust.
///
/// Specialized version of `Wrap` that requires `Deref`, so you can read the underlying `T` even
/// if the wrapper type is polymorphic.
///
/// If your wrapper type doesn't implement `Deref`, you should implement the `Wrap` trait instead.
pub trait WrapRef {
    type WrapRef<'a, T: ?Sized + 'a>: Deref<Target = T>;
}

/// Immutable/shared reference (`&'a T`)
pub struct Immutable;

/// Turn `T` into `&'a T`
impl WrapRef for Immutable {
    type WrapRef<'a, T: ?Sized + 'a> = &'a T;
}

/// Anything implementing `WrapMut` trivially implements `WrapRef`.
impl<R: WrapMut> WrapRef for R {
    type WrapRef<'a, T: ?Sized + 'a> = R::WrapMut<'a, T>;
}

/// Generic exclusive/mutable reference type wrappers. A limited form of higher-kinded types in
/// Rust.
///
/// Specialized version of `Wrap` that requires `DerefMut`, so you can mutate the underlying `T`
/// even if the wrapper type is polymorphic.
///
/// If your wrapper type doesn't implement `DerefMut`, you should implement the `WrapRef` trait
/// instead.
pub trait WrapMut {
    type WrapMut<'a, T: ?Sized + 'a>: Deref<Target = T> + DerefMut;
}

/// Mutable/exclusive reference (`&'a mut T`)
pub struct Mutable;

/// Turn `T` into `&'a mut T`
impl WrapMut for Mutable {
    type WrapMut<'a, T: ?Sized + 'a> = &'a mut T;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::Infallible;

    // Functions are not marked with the `#[test]` attribute because they're just to verify
    // everything type checks.

    fn test_wrap_phantom() {
        let _: <Phantom as Wrap>::Wrap<'_, Infallible> = PhantomData;
    }

    fn test_wrap_immutable() {
        let xs: [usize; 3] = [1, 2, 3];
        let _: <Immutable as Wrap>::Wrap<'_, [usize]> = &xs;
    }

    fn test_wrap_mutable() {
        let mut xs: [usize; 3] = [1, 2, 3];
        let _: <Mutable as Wrap>::Wrap<'_, [usize]> = &mut xs;
    }

    fn test_wrap_deref() {
        let mut x: TestDeref<Vec<usize>> = TestDeref { value: vec![42] };
        let r: <Mutable as Wrap>::Wrap<'_, TestDeref<Vec<usize>>> = &mut x;
        assert_eq!(r.value[0], 42);
    }

    #[test]
    fn test_ref_immutable() {
        let xs: [usize; 3] = [1, 2, 3];
        let _: <Immutable as WrapRef>::WrapRef<'_, [usize]> = &xs;
    }

    #[test]
    fn test_mut_mutable() {
        let mut xs: [usize; 3] = [1, 2, 3];
        let _: <Mutable as WrapMut>::WrapMut<'_, [usize]> = &mut xs;
    }

    struct TestDeref<T> {
        value: T,
    }

    #[test]
    fn test_ref_deref() {
        let mut x: TestDeref<Vec<usize>> = TestDeref { value: vec![42] };
        let r: <Immutable as WrapRef>::WrapRef<'_, TestDeref<Vec<usize>>> = &mut x;
        assert_eq!(r.value[0], 42);
    }

    #[test]
    fn test_mut_deref_mut() {
        let mut x: TestDeref<Vec<usize>> = TestDeref { value: vec![42] };
        let r: <Mutable as WrapMut>::WrapMut<'_, TestDeref<Vec<usize>>> = &mut x;
        r.value[0] = 69;
    }
}
