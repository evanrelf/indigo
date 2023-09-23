{-# LANGUAGE NoFieldSelectors #-}

module Indigo.Core.Buffer.File
  ( FileBuffer

    -- * Create
  , fromFile

    -- * Query
  , path
  , contents
  , selection
  , isModified
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

data FileBuffer = FileBuffer
  { path :: !FilePath
  , contents :: !Rope
  , selection :: !Selection
  , isModified :: !Bool
  , isReadOnly :: !Bool
  , verticalScroll :: {-# UNPACK #-} !LineIndex
  , horizontalScroll :: {-# UNPACK #-} !ColumnIndex
  }

fromFile :: MonadIO m => FilePath -> m FileBuffer
fromFile path = do
  bytes <- readFileBS path
  text <- Exception.fromEither $ decodeUtf8Strict bytes
  pure FileBuffer
    { path
    , contents = Rope.fromText text
    , selection = def
    , isModified = False
    , isReadOnly = undefined
    , verticalScroll = 0
    , horizontalScroll = 0
    }

path :: FileBuffer -> FilePath
path buffer = buffer.path

contents :: FileBuffer -> Rope
contents buffer = buffer.contents

selection :: FileBuffer -> Selection
selection buffer = buffer.selection

isModified :: FileBuffer -> Bool
isModified buffer = buffer.isModified

isReadOnly :: FileBuffer -> Bool
isReadOnly buffer = buffer.isReadOnly

verticalScroll :: FileBuffer -> LineIndex
verticalScroll buffer = buffer.verticalScroll

horizontalScroll :: FileBuffer -> ColumnIndex
horizontalScroll buffer = buffer.horizontalScroll

scrollUp :: Word -> FileBuffer -> FileBuffer
scrollUp distance buffer =
  scrollToLine (buffer.verticalScroll `satSub` distance) buffer
  where satSub = coerce (|-)

scrollDown :: Word -> FileBuffer -> FileBuffer
scrollDown distance buffer =
  scrollToLine (buffer.verticalScroll `add` distance) buffer
  where add = coerce ((+) @Word)

scrollLeft :: Word -> FileBuffer -> FileBuffer
scrollLeft distance buffer =
  scrollToColumn (buffer.horizontalScroll `satSub` distance) buffer
  where satSub = coerce (|-)

scrollRight :: Word -> FileBuffer -> FileBuffer
scrollRight distance buffer =
  scrollToColumn (buffer.horizontalScroll `add` distance) buffer
  where add = coerce ((+) @Word)

scrollToLine :: LineIndex -> FileBuffer -> FileBuffer
scrollToLine line buffer = buffer{ verticalScroll = min line lastLine }
  where
  lastLine = Rope.lengthLines buffer.contents `satSub` (1 :: Word)
  satSub = coerce (|-)

-- TODO: Should this be capped at the length of the longest line?
scrollToColumn :: ColumnIndex -> FileBuffer -> FileBuffer
scrollToColumn column buffer = buffer{ horizontalScroll = column }

-- Selection must be valid in the rope
-- Horizontal scroll offset must not exceed length of longest line(?)
-- Vertical scroll offset must not exceed length of buffer
isValid :: FileBuffer -> Bool
isValid = undefined
