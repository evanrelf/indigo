pub mod btree_map;
pub mod crdt_vibed_length;
pub mod crdt_vibed_not_length;
pub mod document;
pub mod merge;

pub type IndigoHasher = std::hash::DefaultHasher;
pub type IndigoBuildHasher = std::hash::BuildHasherDefault<IndigoHasher>;
