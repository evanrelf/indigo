use crate::edit::{Bias, Edit};
use imbl::Vector;
use ropey::Rope;

pub struct Document {
    id: usize,
    rope: Rope,
    commits: Vector<Commit>,
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
            commits: Vector::new(),
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
    pub fn commits(&self) -> &Vector<Commit> {
        &self.commits
    }

    #[must_use]
    pub fn create_snapshot(&self) -> Snapshot {
        Snapshot {
            version: self.version(),
            rope: self.rope.clone(),
            commits: self.commits.clone(),
        }
    }

    #[must_use]
    pub fn create_draft(&self) -> Draft {
        self.create_snapshot().into_draft()
    }

    pub fn apply_edit(&mut self, edit: Edit) -> anyhow::Result<()> {
        edit.apply(&mut self.rope)?;
        self.commits.push_back(Commit {
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
        self.commits.push_back(Commit {
            original_base_version: draft.base_version,
            original_edit: draft.edit,
            rebased_edit,
        });
        Ok(())
    }

    pub fn create_anchor(&self, byte_index: usize, bias: Bias) -> anyhow::Result<Anchor> {
        Anchor::create(self.version(), &self.rope, byte_index, bias)
    }

    pub fn resolve_anchor(&self, anchor: Anchor) -> anyhow::Result<usize> {
        anchor.resolve(self.version(), &self.commits)
    }

    fn compose_since(&self, base_version: Version) -> anyhow::Result<Edit> {
        // NOTE: Version compatibility must be checked by caller
        let mut commits = self
            .commits
            .focus()
            .narrow(base_version.commit_index..)
            .into_iter();
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

#[derive(Clone)]
pub struct Snapshot {
    version: Version,
    rope: Rope,
    commits: Vector<Commit>,
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
    pub fn into_draft(self) -> Draft {
        Draft {
            base_version: self.version,
            edit: Edit::identity(&self.rope),
            rope: self.rope,
            commits: self.commits,
        }
    }

    #[must_use]
    pub fn create_draft(&self) -> Draft {
        self.clone().into_draft()
    }

    pub fn create_anchor(&self, byte_index: usize, bias: Bias) -> anyhow::Result<Anchor> {
        Anchor::create(self.version, &self.rope, byte_index, bias)
    }

    pub fn resolve_anchor(&self, anchor: Anchor) -> anyhow::Result<usize> {
        anchor.resolve(self.version, &self.commits)
    }
}

pub struct Draft {
    base_version: Version,
    rope: Rope,
    edit: Edit,
    commits: Vector<Commit>,
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

    // No `create_anchor` for `Draft`

    pub fn resolve_anchor(&self, anchor: Anchor) -> anyhow::Result<usize> {
        let byte_index = anchor.resolve(self.base_version, &self.commits)?;
        Ok(self.edit.map_position(byte_index, anchor.bias))
    }
}

#[derive(Clone)]
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

#[derive(Clone, Copy)]
pub struct Anchor {
    base_version: Version,
    byte_index: usize,
    bias: Bias,
}

impl Anchor {
    fn create(
        base_version: Version,
        rope: &Rope,
        byte_index: usize,
        bias: Bias,
    ) -> anyhow::Result<Self> {
        anyhow::ensure!(
            byte_index <= rope.len(),
            "anchor byte index {} is beyond rope length {}",
            byte_index,
            rope.len()
        );
        Ok(Self {
            base_version,
            byte_index,
            bias,
        })
    }

    fn resolve(self, version: Version, commits: &Vector<Commit>) -> anyhow::Result<usize> {
        debug_assert_eq!(version.commit_index, commits.len());
        anyhow::ensure!(
            self.base_version.document_id == version.document_id,
            "anchor does not apply to this document"
        );
        anyhow::ensure!(
            self.base_version.commit_index <= version.commit_index,
            "anchor version is ahead of resolver version"
        );
        let mut byte_index = self.byte_index;
        for commit in commits.focus().narrow(self.base_version.commit_index..) {
            byte_index = commit.rebased_edit.map_position(byte_index, self.bias);
        }
        Ok(byte_index)
    }
}
