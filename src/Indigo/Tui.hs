module Indigo.Tui
  ( main
  )
where

import Brick (App (..))
import Prelude hiding (State, state)

import qualified Brick
import qualified Graphics.Vty as Vty

main :: IO ()
main = do
  _finalState <- Brick.defaultMain app initialState
  pure ()

data State = State
  { message :: !Text
  }

app :: Brick.App State () ()
app =
  App
    { appDraw = draw
    , appChooseCursor = \_state _cursorLocations -> Nothing
    , appHandleEvent = handleEvent
    , appStartEvent = pure ()
    , appAttrMap = \_state -> Brick.attrMap Vty.defAttr []
    }

initialState :: State
initialState =
  State
    { message = "Hello, world!"
    }

draw :: State -> [Brick.Widget ()]
draw state = [Brick.txt state.message]

handleEvent :: Brick.BrickEvent () () -> Brick.EventM () State ()
handleEvent = \case
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) -> Brick.halt
  _ -> pure ()
