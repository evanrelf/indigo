{-# LANGUAGE NoFieldSelectors #-}

module Indigo.Core.Buffer.InMemory
  ( InMemoryBuffer
  , Name (..)

    -- * Create
  , scratch
  , fromText
  , fromRope

    -- * Query
  , name
  , contents
  , selection
  , isReadOnly
  , verticalScroll
  , horizontalScroll

    -- * Modify
  , scrollUp
  , scrollDown
  , scrollLeft
  , scrollRight
  , scrollToLine
  , scrollToColumn

    -- * Consume

    -- * Internal
  , isValid
  )
where

import Data.Default.Class (Default (..))
import Indigo.Core.Rope (ColumnIndex (..), LineIndex (..), Rope)
import Indigo.Core.Rope qualified as Rope
import Indigo.Core.Selection (Selection)
import Indigo.Core.Utilities ((|-))
import UnliftIO.Exception qualified as Exception

data InMemoryBuffer = InMemoryBuffer
  { name :: !Name
  , contents :: !Rope
  , selection :: !Selection
  , isReadOnly :: !Bool
  , verticalScroll :: {-# UNPACK #-} !LineIndex
  , horizontalScroll :: {-# UNPACK #-} !ColumnIndex
  }

newtype Name = Name{ unName :: Text }

scratch :: InMemoryBuffer
scratch = fromRope (Name "scratch") def

fromText :: Name -> Text -> InMemoryBuffer
fromText name text = fromRope name (Rope.fromText text)

fromRope :: Name -> Rope -> InMemoryBuffer
fromRope name rope =
  InMemoryBuffer
    { name
    , contents = rope
    , selection = def
    , isReadOnly = False
    , verticalScroll = 0
    , horizontalScroll = 0
    }

name :: InMemoryBuffer -> Name
name buffer = buffer.name

contents :: InMemoryBuffer -> Rope
contents buffer = buffer.contents

selection :: InMemoryBuffer -> Selection
selection buffer = buffer.selection

isReadOnly :: InMemoryBuffer -> Bool
isReadOnly buffer = buffer.isReadOnly

verticalScroll :: InMemoryBuffer -> LineIndex
verticalScroll buffer = buffer.verticalScroll

horizontalScroll :: InMemoryBuffer -> ColumnIndex
horizontalScroll buffer = buffer.horizontalScroll

scrollUp :: Word -> InMemoryBuffer -> InMemoryBuffer
scrollUp distance buffer =
  scrollToLine (buffer.verticalScroll `satSub` distance) buffer
  where satSub = coerce (|-)

scrollDown :: Word -> InMemoryBuffer -> InMemoryBuffer
scrollDown distance buffer =
  scrollToLine (buffer.verticalScroll `add` distance) buffer
  where add = coerce ((+) @Word)

scrollLeft :: Word -> InMemoryBuffer -> InMemoryBuffer
scrollLeft distance buffer =
  scrollToColumn (buffer.horizontalScroll `satSub` distance) buffer
  where satSub = coerce (|-)

scrollRight :: Word -> InMemoryBuffer -> InMemoryBuffer
scrollRight distance buffer =
  scrollToColumn (buffer.horizontalScroll `add` distance) buffer
  where add = coerce ((+) @Word)

scrollToLine :: LineIndex -> InMemoryBuffer -> InMemoryBuffer
scrollToLine line buffer = buffer{ verticalScroll = min line lastLine }
  where
  lastLine = Rope.lengthLines buffer.contents `satSub` (1 :: Word)
  satSub = coerce (|-)

-- TODO: Should this be capped at the length of the longest line?
scrollToColumn :: ColumnIndex -> InMemoryBuffer -> InMemoryBuffer
scrollToColumn column buffer = buffer{ horizontalScroll = column }

-- Selection must be valid in the rope
-- Horizontal scroll offset must not exceed length of longest line(?)
-- Vertical scroll offset must not exceed length of buffer
isValid :: InMemoryBuffer -> Bool
isValid = undefined
