pub mod buffer;
pub mod cursor;
pub mod display_width;
pub mod editor;
pub mod graphemes;
pub mod history;
pub mod key;
pub mod mode;
pub mod ot;
pub mod pattern;
pub mod prelude;
pub mod range;
pub mod rope;
pub mod text;

#[cfg(feature = "tracy-alloc")]
mod alloc {
    use std::alloc;
    use tracing_tracy::client::ProfiledAllocator;

    #[global_allocator]
    static ALLOC: ProfiledAllocator<alloc::System> = ProfiledAllocator::new(alloc::System, 100);
}
