pub mod actions;
pub mod cursor_unified;
pub mod display_width;
pub mod editor;
pub mod event;
pub mod key;
pub mod mode;
pub mod ot;
pub mod prelude;
pub mod range;
pub mod rope;
pub mod unicode;

#[cfg(feature = "tracy-alloc")]
#[global_allocator]
static GLOBAL: tracing_tracy::client::ProfiledAllocator<std::alloc::System> =
    tracing_tracy::client::ProfiledAllocator::new(std::alloc::System, 100);
