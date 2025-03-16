/*!

Ever wish you could make a wrapper type generic?

```compile_fail
struct Foo<W>(W<usize>);
type SharedFoo = Foo<Arc>; // would become `Foo(Arc<usize>)`
```

Ever wish Rust had "mutability generics"?

```compile_fail
struct Bar<'a, M>(&'a M usize);
type BarRef<'a> = Bar<'a, ref>; // would become `Bar<'a>(&'a usize)`
type BarMut<'a> = Bar<'a, mut>; // would become `Bar<'a>(&'a mut usize)`
```

Well please keep wishing, because I want those too...

But in the meantime, this crate provides a hack to emulate this behavior in userland!

```no_run
# use indigo_wrap::*;
struct Foo<'a, W: Wrap>(W::Wrap<'a, usize>);
type SharedFoo = Foo<'static, WArc>; // actually becomes `Foo(Arc<usize>)`
```

```no_run
# use indigo_wrap::*;
struct Bar<'a, W: WrapRef>(W::WrapRef<'a, usize>);
type BarRef<'a> = Bar<'a, WRef>; // actually becomes `Bar<'a>(&'a usize)`
type BarMut<'a> = Bar<'a, WMut>; // actually becomes `Bar<'a>(&'a mut usize)`
```

This is a limited form of higher-kinded types, like you find in languages like Haskell.

## Example

Here's an example of a cursor type like you might find in a simple text editor. It maintains its own
state, but whether or not it's valid is in relation to the string it's an offset in.

<details>

<summary>Show code</summary>

```no_run
# use indigo_wrap::*;
type RawCursor = CursorView<'static, WPhantomData>;

type Cursor<'a> = CursorView<'a, WRef>;

type CursorMut<'a> = CursorView<'a, WMut>;

// Use the weakest trait bound `Wrap` when defining the type to perform wrapping:
struct CursorView<'a, W: Wrap> {
    text: W::Wrap<'a, String>,
    byte_offset: usize,
}

// ...or when you don't need to touch the wrapped type:
impl<W: Wrap> CursorView<'_, W> {
    fn reset(&mut self) {
        self.byte_offset = 0;
    }
}

// Use the stronger trait bound `WrapRef` if you need to read values:
impl<W: WrapRef> CursorView<'_, W> {
    fn is_empty(&self) -> bool {
        // We can call `String::is_empty` transparently because `WrapRef` implies `Deref`:
        self.text.is_empty()
    }
}

// Use the strongest trait bound `WrapMut` if you need to mutate values:
impl<W: WrapMut> CursorView<'_, W> {
    fn insert(&mut self, text: &str) {
        // We can call `String::insert_str` transparently because `WrapMut` implies `DerefMut`:
        self.text.insert_str(self.byte_offset, text);
    }
}
```

</details>

There are certain operations you can perform:

- ...without seeing the string (e.g. `RawCursor` could safely set `byte_offset` to 0).
- ...while holding an immutable reference (e.g. checking whether the string is empty).
- ...while holding a mutable reference (e.g. inserting text into the string at the byte offset).

These are captured with the `Wrap`, `WrapRef`, and `WrapMut` traits respectively.

If you write generic `impl` blocks with trait bounds on the wrapper type (e.g.
`impl<W: WrapMut> CursorView<'_, W>`) rather than hardcoding it (e.g. `impl CursorView<'_, WMut>`),
you get a kind of subtyping/inheritance thing where the most powerful versions of your type (e.g.
`CursorMut`) have all the methods available, but your weaker versions (e.g. `Cursor`) only get the
subset that apply to their wrapped type.

*/

use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    rc::Rc,
    sync::Arc,
};

/// Generic type wrappers
///
/// This is a generalized version of [`WrapRef`] that doesn't require [`Deref`], so you can use
/// wrappers that may not have the type they're wrapping.
///
/// If your wrapper type implements [`Deref`], you should implement the [`WrapRef`] trait instead,
/// and you'll get a [`Wrap`] impl for free.
pub trait Wrap {
    type Wrap<'a, T: ?Sized + 'a>;
}

/// Turn `T` into `PhantomData<T>`
pub struct WPhantomData;

/// Turn `T` into `PhantomData<T>`
impl Wrap for WPhantomData {
    type Wrap<'a, T: ?Sized + 'a> = PhantomData<T>;
}

/// Anything implementing [`WrapRef`] trivially implements [`Wrap`].
impl<R: WrapRef> Wrap for R {
    type Wrap<'a, T: ?Sized + 'a> = R::WrapRef<'a, T>;
}

/// Generic immutable/shared reference type wrappers
///
/// Specialized version of `Wrap` that requires [`Deref`], so you can read the underlying `T` even
/// if the wrapper type is polymorphic.
///
/// If your wrapper type doesn't implement [`Deref`], you should implement the [`Wrap`] trait
/// instead.
pub trait WrapRef {
    type WrapRef<'a, T: ?Sized + 'a>: Deref<Target = T>;
}

/// Turn `T` into `PhantomData<T>`
pub struct WRef;

/// Turn `T` into `&'a T`
impl WrapRef for WRef {
    type WrapRef<'a, T: ?Sized + 'a> = &'a T;
}

/// Turn `T` into `Rc<T>`
pub struct WRc;

/// Turn `T` into `Rc<T>`
impl WrapRef for WRc {
    type WrapRef<'a, T: ?Sized + 'a> = Rc<T>;
}

/// Turn `T` into `Arc<T>`
pub struct WArc;

/// Turn `T` into `Arc<T>`
impl WrapRef for WArc {
    type WrapRef<'a, T: ?Sized + 'a> = Arc<T>;
}

/// Anything implementing [`WrapMut`] trivially implements [`WrapRef`].
impl<R: WrapMut> WrapRef for R {
    type WrapRef<'a, T: ?Sized + 'a> = R::WrapMut<'a, T>;
}

/// Generic exclusive/mutable reference type wrappers
///
/// Specialized version of [`Wrap`] that requires [`DerefMut`], so you can mutate the underlying `T`
/// even if the wrapper type is polymorphic.
///
/// If your wrapper type doesn't implement [`DerefMut`], you should implement the [`WrapRef`] trait
/// instead.
pub trait WrapMut {
    type WrapMut<'a, T: ?Sized + 'a>: Deref<Target = T> + DerefMut;
}

/// Turn `T` into `&'a mut T`
pub struct WMut;

/// Turn `T` into `&'a mut T`
impl WrapMut for WMut {
    type WrapMut<'a, T: ?Sized + 'a> = &'a mut T;
}

/// Turn `T` into `Box<T>`
pub struct WBox;

/// Turn `T` into `Box<T>`
impl WrapMut for WBox {
    type WrapMut<'a, T: ?Sized + 'a> = Box<T>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::Infallible;

    // Functions are not marked with the `#[test]` attribute because they're just to verify
    // everything type checks.

    struct TestDeref<T> {
        value: T,
    }

    fn test_wrap() {
        let _: <WPhantomData as Wrap>::Wrap<'_, Infallible> = PhantomData;

        let xs: [usize; 3] = [1, 2, 3];
        let _: <WRef as Wrap>::Wrap<'_, [usize]> = &xs;

        let mut xs: [usize; 3] = [1, 2, 3];
        let _: <WMut as Wrap>::Wrap<'_, [usize]> = &mut xs;

        let mut x: TestDeref<Vec<usize>> = TestDeref { value: vec![42] };
        let r: <WMut as Wrap>::Wrap<'_, TestDeref<Vec<usize>>> = &mut x;
        assert_eq!(r.value[0], 42);
    }

    #[test]
    fn test_wrap_ref() {
        let xs: [usize; 3] = [1, 2, 3];
        let _: <WRef as WrapRef>::WrapRef<'_, [usize]> = &xs;

        let mut x: TestDeref<Vec<usize>> = TestDeref { value: vec![42] };
        let r: <WRef as WrapRef>::WrapRef<'_, TestDeref<Vec<usize>>> = &mut x;
        assert_eq!(r.value[0], 42);
    }

    #[test]
    fn test_wrap_mut() {
        let mut xs: [usize; 3] = [1, 2, 3];
        let _: <WMut as WrapMut>::WrapMut<'_, [usize]> = &mut xs;

        let xs: [usize; 3] = [1, 2, 3];
        let mut bxs: <WBox as WrapMut>::WrapMut<'_, [usize]> = Box::new(xs);
        bxs[1] = 42;
        assert_eq!(bxs[1], 42);

        let mut x: TestDeref<Vec<usize>> = TestDeref { value: vec![42] };
        let r: <WMut as WrapMut>::WrapMut<'_, TestDeref<Vec<usize>>> = &mut x;
        r.value[0] = 69;
    }
}
