module Indigo.Tui
  ( main
  )
where

import Brick (App (..))
import Data.Default.Class (Default (..))
import Indigo.Core.Editor (Editor (..))
import Prelude hiding (State, state)

import qualified Brick
import qualified Graphics.Vty as Vty

main :: IO ()
main = do
  _ <- Brick.defaultMain app def
  pure ()

data State = State
  { editor :: !Editor
  }

instance Default State where
  def :: State
  def =
    State
      { editor = def
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

draw :: State -> [Brick.Widget ()]
draw state = [Brick.txt "Hello, world!"]

handleEvent :: Brick.BrickEvent () () -> Brick.EventM () State ()
handleEvent = \case
  Brick.VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) -> Brick.halt
  _ -> pure ()
