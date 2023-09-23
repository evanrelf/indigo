module Indigo.Core.Editor
  ( Editor (..)
  , Buffers (..)
  )
where

import Data.Default.Class (Default (..))
import Data.Sequence qualified as Seq
import Indigo.Core.Buffer (Buffer)
import Indigo.Core.Buffer qualified as Buffer
import Indigo.Core.Buffer.InMemory qualified as InMemoryBuffer
import Indigo.Core.Mode (Mode)

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
      , current = Buffer.InMemory InMemoryBuffer.scratch
      , after = Seq.empty
      }
