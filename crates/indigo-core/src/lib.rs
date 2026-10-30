pub mod buffer;
pub mod client;
pub mod cursor;
pub mod fs;
pub mod history;
pub mod mode;
pub mod path;
pub mod prelude;
pub mod range;
pub mod selection;
pub mod server;
pub mod session;
pub mod syntax;
pub mod text;
pub mod window;

pub type IndigoHasher = std::hash::DefaultHasher;
pub type IndigoBuildHasher = std::hash::BuildHasherDefault<IndigoHasher>;
