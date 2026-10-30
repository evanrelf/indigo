use camino::{Utf8Component, Utf8Path, Utf8PathBuf};
use pathdiff::diff_utf8_paths;
use std::borrow::Cow;

#[must_use]
pub fn display_path<'a>(
    home: Option<&Utf8Path>,
    pwd: Option<&Utf8Path>,
    path: &'a Utf8Path,
) -> Cow<'a, str> {
    // If there's no `$PWD`, use the path verbatim, because we have nothing to go on.
    let Some(pwd) = pwd else {
        return Cow::Borrowed(path.as_str());
    };
    // If we're under `$PWD`, use a relative path.
    let absolute = normalize(&pwd.join(path));
    if let Ok(relative) = absolute.strip_prefix(pwd) {
        return Cow::Owned(relative.to_string());
    }
    // If we're outside `$PWD`, use the shortest option (preferring absolute, then `~`, on ties).
    let mut shortest = absolute.clone();
    if let Some(home) = home
        && let Ok(relative) = absolute.strip_prefix(home)
    {
        let tilde = Utf8Path::new("~").join(relative);
        if tilde.as_str().len() < shortest.as_str().len() {
            shortest = tilde;
        }
    }
    if let Some(relative) = diff_utf8_paths(&absolute, pwd)
        && relative.as_str().len() < shortest.as_str().len()
    {
        shortest = relative;
    }
    Cow::Owned(shortest.into_string())
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
            // Paths under `$PWD` are shown relative to it.
            ("/me/src/indigo", "/me/src/indigo/Cargo.toml", "Cargo.toml"),
            ("/me/src/indigo", "./src/../Cargo.toml", "Cargo.toml"),
            ("/me/src/indigo", "../indigo/Cargo.toml", "Cargo.toml"),
            // Paths outside `$PWD` are shown as whichever is shortest: relative to `$PWD`,
            // relative to `~`, or absolute (preferring absolute, then `~`, on ties).
            ("/me/src/indigo", "/etc/shells", "/etc/shells"),
            ("/me/src/indigo", "../../../etc/shells", "/etc/shells"),
            ("/me/src/indigo", "/me/src/kak/README", "../kak/README"),
            ("/me/src/indigo", "../kak/README", "../kak/README"),
            ("/me/src/indigo", "../../Library/foo", "~/Library/foo"),
            ("/", "me/foo", "me/foo"),
        ] {
            let display_path = display_path(Some("/me".into()), Some(pwd.into()), path.into());
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
        let home = normalize(Utf8Path::new(&tc.draw(gen_path("/"))));
        let pwd = normalize(Utf8Path::new(&tc.draw(gen_path("/"))));
        let path = tc.draw(gen_path(tc.draw(gs::sampled_from(vec!["", "/"]))));
        let absolute = normalize(&pwd.join(&path));

        let display_path = display_path(Some(&home), Some(&pwd), path.as_str().into());

        let resolved = match display_path.strip_prefix("~") {
            Some(rest) => home.join(rest.trim_start_matches('/')),
            None => pwd.join(display_path.as_ref()),
        };
        assert_eq!(normalize(&resolved), absolute);
        assert!(display_path.len() <= absolute.as_str().len());
        if absolute.starts_with(&pwd) {
            assert!(!display_path.starts_with(['/', '~']) && !display_path.contains(".."));
        }
    }
}
