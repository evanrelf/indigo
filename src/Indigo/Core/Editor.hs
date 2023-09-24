{-# LANGUAGE NoFieldSelectors #-}

module Indigo.Core.Editor
  ( Editor (..)

    -- * Create

    -- * Query
  , buffers
  , mode

    -- * Modify
  , addBuffer

    -- * Consume

    -- * Internal
  )
where

import Data.Default.Class (Default (..))
import Indigo.Core.Buffer (Buffer)
import Indigo.Core.Buffer qualified as Buffer
import Indigo.Core.Buffer.InMemory qualified as InMemoryBuffer
import Indigo.Core.Mode (Mode)

-- TODO: No way of referring to a particular buffer (no IDs, names/paths aren't
-- guaranteed to be unique, no stable ordering)
data Editor = Editor
  { buffers :: ![Buffer]
  , mode :: !Mode
  }

instance Default Editor where
  def :: Editor
  def =
    Editor
      { buffers = [Buffer.InMemory InMemoryBuffer.scratch]
      , mode = def
      }

buffers :: Editor -> [Buffer]
buffers editor = editor.buffers

mode :: Editor -> Mode
mode editor = editor.mode

addBuffer :: Buffer -> Editor -> Editor
addBuffer buffer editor = editor{ buffers = buffer : editor.buffers }
