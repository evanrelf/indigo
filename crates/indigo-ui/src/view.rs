mod hsplit;
mod text;
mod vsplit;

pub use crate::view::{hsplit::*, text::*, vsplit::*};
use xilem_core::{ViewElement, ViewId, ViewPathTracker};

pub struct Context {
    pub path: Vec<ViewId>,
}

impl ViewPathTracker for Context {
    fn push_id(&mut self, id: ViewId) {
        self.path.push(id);
    }

    fn pop_id(&mut self) {
        self.path.pop();
    }

    fn view_path(&mut self) -> &[ViewId] {
        &self.path
    }
}

pub struct Element<T>(T);

impl<T> ViewElement for Element<T>
where
    T: 'static,
{
    type Mut<'a> = &'a mut Self;
}
