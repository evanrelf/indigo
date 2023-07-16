module Indigo.Core.Editor
  ( Editor (..)
  , Buffers (..)
  )
where

import Data.Default.Class (Default (..))
import Indigo.Core.Buffer (Buffer)
import Indigo.Core.Mode (Mode)

import qualified Data.Sequence as Seq

data Editor = Editor
  { buffers :: !Buffers
  , mode :: !Mode
  }

instance Default Editor where
  def :: Editor
  def =
    Editor
      { buffers = def
      , mode = def
      }

data Buffers = Buffers
  { before :: !(Seq Buffer)
  , current :: !Buffer
  , after :: !(Seq Buffer)
  }

instance Default Buffers where
  def :: Buffers
  def =
    Buffers
      { before = Seq.empty
      , current = def
      , after = Seq.empty
      }
