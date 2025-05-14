use crate::view::{Context, Element};
use xilem_core::{DynMessage, MessageResult, Mut, View, ViewId, ViewMarker};

pub struct VSplit<Left, Right> {
    left: Left,
    right: Right,
}

pub fn vsplit<Left, Right>(left: Left, right: Right) -> VSplit<Left, Right> {
    VSplit { left, right }
}

impl<Left, Right> ViewMarker for VSplit<Left, Right> {}

impl<State, Action, Left, Right> View<State, Action, Context> for VSplit<Left, Right>
where
    Left: 'static,
    Right: 'static,
{
    type Element = Element<Self>;

    type ViewState = ();

    fn build(&self, _ctx: &mut Context) -> (Self::Element, Self::ViewState) {
        todo!()
    }

    fn rebuild(
        &self,
        _prev: &Self,
        _view_state: &mut Self::ViewState,
        _ctx: &mut Context,
        _element: Mut<'_, Self::Element>,
    ) {
        todo!()
    }

    fn teardown(
        &self,
        _view_state: &mut Self::ViewState,
        _ctx: &mut Context,
        _element: Mut<'_, Self::Element>,
    ) {
        todo!()
    }

    fn message(
        &self,
        _view_state: &mut Self::ViewState,
        _id_path: &[ViewId],
        _message: DynMessage,
        _app_state: &mut State,
    ) -> MessageResult<Action, DynMessage> {
        todo!()
    }
}
