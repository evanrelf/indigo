pub mod action;
pub mod buffer;
pub mod cursor;
pub mod display_width;
pub mod editor;
pub mod event;
pub mod graphemes;
pub mod key;
pub mod mode;
pub mod ot;
pub mod prelude;
pub mod range;
pub mod rope;

#[cfg(feature = "tracy-alloc")]
mod alloc {
    use std::alloc;
    use tracing_tracy::client::ProfiledAllocator;

    #[global_allocator]
    static ALLOC: ProfiledAllocator<alloc::System> = ProfiledAllocator::new(alloc::System, 100);
}
