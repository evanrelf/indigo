use crate::view::{Context, Element};
use std::sync::Arc;
use xilem_core::{DynMessage, MessageResult, Mut, View, ViewId, ViewMarker};

#[derive(Clone)]
pub struct Text {
    content: Arc<str>,
}

pub fn text(content: impl Into<Arc<str>>) -> Text {
    Text {
        content: content.into(),
    }
}

impl ViewMarker for Text {}

impl<State, Action> View<State, Action, Context> for Text {
    type Element = Element<Self>;

    type ViewState = ();

    fn build(&self, _ctx: &mut Context) -> (Self::Element, Self::ViewState) {
        let text = self.clone();
        (Element(text), ())
    }

    fn rebuild(
        &self,
        prev: &Self,
        _view_state: &mut Self::ViewState,
        _ctx: &mut Context,
        element: Mut<'_, Self::Element>,
    ) {
        if prev.content != self.content {
            element.0.content = self.content.clone();
        }
    }

    fn teardown(
        &self,
        _view_state: &mut Self::ViewState,
        _ctx: &mut Context,
        _element: Mut<'_, Self::Element>,
    ) {
    }

    fn message(
        &self,
        _view_state: &mut Self::ViewState,
        _id_path: &[ViewId],
        message: DynMessage,
        _app_state: &mut State,
    ) -> MessageResult<Action, DynMessage> {
        MessageResult::Stale(message)
    }
}
