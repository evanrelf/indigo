use anyhow::anyhow;
use camino::{Utf8Path, Utf8PathBuf};
use std::collections::HashMap;

// Needs to remain dyn compatible.
pub trait Fs {
    fn read_file(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>>;

    fn write_file(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()>;

    fn file_exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool>;
}

impl<T: Fs + ?Sized> Fs for Box<T> {
    fn read_file(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        T::read_file(self, path)
    }
    fn write_file(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        T::write_file(self, path, bytes)
    }
    fn file_exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool> {
        T::file_exists(self, path)
    }
}

// Trivial filesystem implementation that panics if you do anything. This is the default filesystem
// implementation for `Editor`.
pub struct NoFs;

impl Fs for NoFs {
    fn read_file(&mut self, _path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        panic!("No filesystem implementation configured");
    }
    fn write_file(&mut self, _path: &Utf8Path, _bytes: &[u8]) -> anyhow::Result<()> {
        panic!("No filesystem implementation configured");
    }
    fn file_exists(&mut self, _path: &Utf8Path) -> anyhow::Result<bool> {
        panic!("No filesystem implementation configured");
    }
}

#[cfg_attr(not(test), expect(dead_code))]
#[derive(Default)]
pub(crate) struct TestFs(pub HashMap<Utf8PathBuf, Vec<u8>>);

impl Fs for TestFs {
    fn read_file(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        if let Some(bytes) = self.0.get(path) {
            Ok(bytes.clone())
        } else {
            Err(anyhow!("File not found: `{path}`"))
        }
    }

    fn write_file(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        self.0.insert(path.to_path_buf(), bytes.to_vec());
        Ok(())
    }

    fn file_exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool> {
        let exists = self.0.contains_key(path);
        Ok(exists)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() -> anyhow::Result<()> {
        let mut fs = TestFs::default();
        fs.write_file("foo.rs".into(), b"fn foo() {}")?;
        fs.write_file("bar.rs".into(), b"fn bar() {}")?;
        assert!(fs.file_exists("foo.rs".into())?);
        assert_eq!(&fs.read_file("foo.rs".into())?, b"fn foo() {}");
        assert_eq!(&fs.read_file("bar.rs".into())?, b"fn bar() {}");
        assert!(fs.read_file("baz.rs".into()).is_err());
        assert!(!fs.file_exists("baz.rs".into())?);
        fs.write_file("foo.rs".into(), b"fn foo() { panic!() }")?;
        assert_eq!(&fs.read_file("foo.rs".into())?, b"fn foo() { panic!() }");
        Ok(())
    }
}
