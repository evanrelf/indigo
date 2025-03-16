pub mod action;
pub mod buffer;
pub mod cursor;
pub mod cursor_view;
pub mod display_width;
pub mod editor;
pub mod event;
pub mod graphemes;
pub mod key;
pub mod mode;
pub mod ot;
pub mod prelude;
pub mod range;
pub mod range_view;
pub mod rope;

#[cfg(feature = "tracy-alloc")]
#[global_allocator]
static GLOBAL: tracing_tracy::client::ProfiledAllocator<std::alloc::System> =
    tracing_tracy::client::ProfiledAllocator::new(std::alloc::System, 100);
