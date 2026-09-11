// Finished, used by `indigo-core`
pub mod edit;
pub mod grapheme;

// Under construction
pub mod crdt_vibed_length;
pub mod crdt_vibed_not_length;
pub mod document;
pub mod merge;

#[expect(unsafe_code)]
#[unsafe(no_mangle)]
pub extern "C" fn add(a: i32, b: i32) -> i32 {
    a + b
}
