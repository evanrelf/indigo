use crate::edit::{Bias, Edit};
use ropey::Rope;

pub struct Document {
    id: usize,
    rope: Rope,
    commits: Vec<Commit>,
}

impl Document {
    #[must_use]
    pub fn new(id: usize) -> Self {
        Self::from_rope(id, Rope::new())
    }

    #[must_use]
    pub fn from_str(id: usize, str: &str) -> Self {
        Self::from_rope(id, Rope::from(str))
    }

    #[must_use]
    pub fn from_rope(id: usize, rope: Rope) -> Self {
        Self {
            id,
            rope,
            commits: Vec::new(),
        }
    }

    #[must_use]
    pub fn version(&self) -> Version {
        Version {
            document_id: self.id,
            commit_index: self.commits.len(),
        }
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    #[must_use]
    pub fn commits(&self) -> &[Commit] {
        &self.commits
    }

    #[must_use]
    pub fn snapshot(&self) -> Snapshot {
        Snapshot {
            version: self.version(),
            rope: self.rope.clone(),
        }
    }

    #[must_use]
    pub fn draft(&self) -> Draft {
        Draft {
            base_version: self.version(),
            rope: self.rope.clone(),
            edit: Edit::identity(&self.rope),
        }
    }

    pub fn apply_edit(&mut self, edit: Edit) -> anyhow::Result<()> {
        edit.apply(&mut self.rope)?;
        self.commits.push(Commit {
            original_base_version: self.version(),
            original_edit: edit.clone(),
            rebased_edit: edit,
        });
        Ok(())
    }

    pub fn apply_draft(&mut self, draft: Draft) -> anyhow::Result<()> {
        self.assert_compatible_version(draft.base_version)?;
        let concurrent_edit = self.compose_since(draft.base_version)?;
        let rebased_edit = draft.edit.rebase(&concurrent_edit, Bias::Forward)?;
        rebased_edit.apply(&mut self.rope)?;
        self.commits.push(Commit {
            original_base_version: draft.base_version,
            original_edit: draft.edit,
            rebased_edit,
        });
        Ok(())
    }

    fn compose_since(&self, base_version: Version) -> anyhow::Result<Edit> {
        // NOTE: Version compatibility must be checked by caller
        let mut commits = self.commits[base_version.commit_index..].iter();
        if let Some(first_commit) = commits.next() {
            let mut edit = first_commit.rebased_edit.clone();
            for commit in commits {
                edit = edit.compose(&commit.rebased_edit)?;
            }
            Ok(edit)
        } else {
            Ok(Edit::identity(&self.rope))
        }
    }

    fn assert_compatible_version(&self, version: Version) -> anyhow::Result<()> {
        anyhow::ensure!(
            version.document_id == self.version().document_id,
            "version does not apply to this document"
        );
        anyhow::ensure!(
            version.commit_index <= self.version().commit_index,
            "version is ahead of document version"
        );
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Version {
    document_id: usize,
    commit_index: usize,
}

pub struct Snapshot {
    version: Version,
    rope: Rope,
}

impl Snapshot {
    #[must_use]
    pub fn version(&self) -> Version {
        self.version
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    #[must_use]
    pub fn draft(&self) -> Draft {
        Draft {
            base_version: self.version,
            rope: self.rope.clone(),
            edit: Edit::identity(&self.rope),
        }
    }
}

pub struct Draft {
    base_version: Version,
    rope: Rope,
    edit: Edit,
}

impl Draft {
    #[must_use]
    pub fn base_version(&self) -> Version {
        self.base_version
    }

    #[must_use]
    pub fn rope(&self) -> &Rope {
        &self.rope
    }

    #[must_use]
    pub fn edit(&self) -> &Edit {
        &self.edit
    }

    pub fn apply_edit(&mut self, edit: &Edit) -> anyhow::Result<()> {
        let composed = self.edit.compose(edit)?;
        edit.apply(&mut self.rope)?;
        self.edit = composed;
        Ok(())
    }
}

pub struct Commit {
    original_base_version: Version,
    original_edit: Edit,
    rebased_edit: Edit,
}

impl Commit {
    #[must_use]
    pub fn original_base_version(&self) -> Version {
        self.original_base_version
    }

    #[must_use]
    pub fn original_edit(&self) -> &Edit {
        &self.original_edit
    }

    #[must_use]
    pub fn rebased_edit(&self) -> &Edit {
        &self.rebased_edit
    }
}
