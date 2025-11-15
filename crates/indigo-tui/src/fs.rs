use camino::Utf8Path;
use indigo_core::fs::Fs;
use std::fs;

pub struct TuiFs;

impl Fs for TuiFs {
    fn read_file(&mut self, path: &Utf8Path) -> anyhow::Result<Vec<u8>> {
        Ok(fs::read(path)?)
    }

    fn write_file(&mut self, path: &Utf8Path, bytes: &[u8]) -> anyhow::Result<()> {
        Ok(fs::write(path, bytes)?)
    }

    fn file_exists(&mut self, path: &Utf8Path) -> anyhow::Result<bool> {
        Ok(fs::exists(path)?)
    }
}
