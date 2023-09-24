module Indigo.Tui
  ( run
  )
where

import Brick (App (..))
import Brick qualified
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Indigo.Core.Buffer qualified as Buffer
import Indigo.Core.Editor (Editor (..))
import Indigo.Core.Editor qualified as Editor
import Indigo.Core.Rope qualified as Rope
import Prelude hiding (State, state)
import Relude.Unsafe qualified as Unsafe

run :: MonadIO m => Editor -> m ()
run editor = do
  let initialState = State{ editor }
  void $ liftIO $ Brick.defaultMain app initialState

data State = State
  { editor :: !Editor
  }

app :: Brick.App State Void ()
app =
  App
    { appDraw = draw
    , appChooseCursor = \_state _cursorLocations -> Nothing
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = \_state -> Brick.attrMap Vty.defAttr []
    }

draw :: State -> [Brick.Widget ()]
draw state =
  [ state.editor
    & Editor.buffers
    & Unsafe.head
    & Buffer.contents
    & Rope.toText
    & Text.replace "\t" (Text.replicate 8 " ")
    & Brick.txt
  ]

handleEvent :: Brick.BrickEvent () Void -> Brick.EventM () State ()
handleEvent = \case
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) -> Brick.halt
  _ -> pure ()
