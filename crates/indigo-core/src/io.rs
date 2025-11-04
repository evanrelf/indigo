use anyhow::anyhow;
use camino::{Utf8Path, Utf8PathBuf};
use std::collections::HashMap;

// Needs to remain dyn compatible.
pub trait Io {
    fn read_file(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>>;

    fn write_file(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()>;

    fn file_exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool>;
}

#[cfg_attr(not(test), expect(dead_code))]
#[derive(Default)]
pub(crate) struct TestIo {
    filesystem: HashMap<Utf8PathBuf, Vec<u8>>,
}

impl Io for TestIo {
    fn read_file(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        if let Some(bytes) = self.filesystem.get(path) {
            Ok(bytes.clone())
        } else {
            Err(anyhow!("File not found: `{path}`"))
        }
    }

    fn write_file(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        self.filesystem.insert(path.to_path_buf(), bytes.to_vec());
        Ok(())
    }

    fn file_exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool> {
        let exists = self.filesystem.contains_key(path);
        Ok(exists)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> anyhow::Result<()> {
        let mut io = TestIo::default();
        io.write_file("foo.rs".into(), b"fn foo() {}")?;
        io.write_file("bar.rs".into(), b"fn bar() {}")?;
        assert!(io.file_exists("foo.rs".into())?);
        assert_eq!(&io.read_file("foo.rs".into())?, b"fn foo() {}");
        assert_eq!(&io.read_file("bar.rs".into())?, b"fn bar() {}");
        assert!(io.read_file("baz.rs".into()).is_err());
        assert!(!io.file_exists("baz.rs".into())?);
        io.write_file("foo.rs".into(), b"fn foo() { panic!() }")?;
        assert_eq!(&io.read_file("foo.rs".into())?, b"fn foo() { panic!() }");
        Ok(())
    }
}
