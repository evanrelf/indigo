use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
use std::borrow::Cow;

/// Path for showing to the user. Paths under the working directory are shown relative to it, and
/// other paths are shown absolute.
#[must_use]
pub fn display_path<'a>(path: &'a Utf8Path, pwd: Option<&Utf8Path>) -> Cow<'a, str> {
    let Some(pwd) = pwd else {
        return Cow::Borrowed(path.as_str());
    };
    let absolute = normalize(&pwd.join(path));
    match absolute.strip_prefix(pwd) {
        Ok(relative) => Cow::Owned(relative.to_string()),
        Err(_) => Cow::Owned(absolute.into_string()),
    }
}

/// Resolve `.` and `..` components without touching the filesystem, preserving symlinks.
fn normalize(path: &Utf8Path) -> Utf8PathBuf {
    let mut normalized = Utf8PathBuf::new();
    for component in path.components() {
        match component {
            Utf8Component::CurDir => {}
            Utf8Component::ParentDir => _ = normalized.pop(),
            component => normalized.push(component),
        }
    }
    normalized
}

#[cfg(test)]
mod tests {
    use super::*;
    use hegel::generators as gs;

    #[test]
    fn test_display_path() {
        for (pwd, path, expected) in [
            ("/me/src/indigo", "/etc/shells", "/etc/shells"),
            ("/me/src/indigo", "/me/src/indigo/Cargo.toml", "Cargo.toml"),
            ("/me/src/indigo", "./src/../Cargo.toml", "Cargo.toml"),
            ("/me/src/indigo", "../indigo/Cargo.toml", "Cargo.toml"),
            ("/me/src/indigo", "../../../etc/shells", "/etc/shells"),
            ("/me/src/indigo", "/me/src/kak/README", "/me/src/kak/README"),
        ] {
            let display_path = display_path(path.into(), Some(pwd.into()));
            assert_eq!(display_path, expected, "{path} in {pwd}");
        }
    }

    #[hegel::composite]
    fn gen_path(tc: &hegel::TestCase, root: &'static str) -> String {
        let components = gs::vecs(gs::sampled_from(vec!["a", "b", ".", ".."])).max_size(6);
        root.to_owned() + &tc.draw(components).join("/")
    }

    #[hegel::test(test_cases = 5_000)]
    fn display_path_points_to_same_place(tc: hegel::TestCase) {
        let pwd = normalize(Utf8Path::new(&tc.draw(gen_path("/"))));
        let path = tc.draw(gen_path(tc.draw(gs::sampled_from(vec!["", "/"]))));
        let absolute = normalize(&pwd.join(&path));

        let display_path = display_path(path.as_str().into(), Some(&pwd));

        assert_eq!(normalize(&pwd.join(display_path.as_ref())), absolute);
        assert!(display_path.len() <= absolute.as_str().len());
        if absolute.starts_with(&pwd) {
            assert!(!display_path.starts_with('/') && !display_path.contains(".."));
        }
    }
}
