module Indigo.Core.Editor
  ( Editor (..)
  )
where

import Data.Default.Class (Default (..))
import Indigo.Core.Buffer (Buffer)
import Indigo.Core.Mode (Mode)

data Editor = Editor
  { buffers :: !(NonEmpty Buffer)
  , mode :: !Mode
  }

instance Default Editor where
  def :: Editor
  def =
    Editor
      { buffers = one def
      , mode = def
      }
