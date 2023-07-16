{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.Buffer
  ( Buffer

    -- * Create
  , fromText
  , fromRope
  , fromFile

    -- * Query
  , path
  , contents
  , selection
  , isModified
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
import Indigo.Core.Selection (Selection)
import Indigo.Core.Utilities ((|-), (+|))
import Prelude hiding (empty)

import qualified Indigo.Core.Rope as Rope
import qualified UnliftIO.Exception as Exception

-- TODO: Scratch/virtual buffers without modification state
data Buffer = Buffer
  { path :: !(Maybe FilePath)
  , contents :: !Rope
  , selection :: !Selection
  , isModified :: !Bool
  , verticalScroll :: {-# UNPACK #-} !LineIndex
  , horizontalScroll :: {-# UNPACK #-} !ColumnIndex
  }

instance Default Buffer where
  def :: Buffer
  def =
    Buffer
      { path = Nothing
      , contents = mempty
      , selection = def
      , isModified = False
      , verticalScroll = 0
      , horizontalScroll = 0
      }

fromText :: Text -> Buffer
fromText = fromRope . Rope.fromText

fromRope :: Rope -> Buffer
fromRope contents = def{ contents }

fromFile :: MonadIO m => FilePath -> m Buffer
fromFile path = do
  bytes <- readFileBS path
  text <- Exception.fromEither $ decodeUtf8Strict bytes
  pure def
    { path = Just path
    , contents = Rope.fromText text
    }

path :: Buffer -> Maybe FilePath
path buffer = buffer.path

contents :: Buffer -> Rope
contents buffer = buffer.contents

selection :: Buffer -> Selection
selection buffer = buffer.selection

isModified :: Buffer -> Bool
isModified buffer = buffer.isModified

verticalScroll :: Buffer -> LineIndex
verticalScroll buffer = buffer.verticalScroll

horizontalScroll :: Buffer -> ColumnIndex
horizontalScroll buffer = buffer.horizontalScroll

scrollUp :: Word -> Buffer -> Buffer
scrollUp distance buffer =
  scrollToLine (buffer.verticalScroll `satSub` distance) buffer
  where satSub = coerce (|-)

scrollDown :: Word -> Buffer -> Buffer
scrollDown distance buffer =
  scrollToLine (buffer.verticalScroll `satAdd` distance) buffer
  where satAdd = coerce (+|)

scrollLeft :: Word -> Buffer -> Buffer
scrollLeft distance buffer =
  scrollToColumn (buffer.horizontalScroll `satSub` distance) buffer
  where satSub = coerce (|-)

scrollRight :: Word -> Buffer -> Buffer
scrollRight distance buffer =
  scrollToColumn (buffer.horizontalScroll `satAdd` distance) buffer
  where satAdd = coerce (+|)

scrollToLine :: LineIndex -> Buffer -> Buffer
scrollToLine line buffer = buffer{ verticalScroll = min line lastLine }
  where
  lastLine = Rope.lengthLines buffer.contents `satSub` (1 :: Word)
  satSub = coerce (|-)

-- TODO: Should this be capped at the length of the longest line?
scrollToColumn :: ColumnIndex -> Buffer -> Buffer
scrollToColumn column buffer = buffer{ horizontalScroll = column }

-- Selection must be valid in the rope
-- Horizontal scroll offset must not exceed length of longest line(?)
-- Vertical scroll offset must not exceed length of buffer
isValid :: Buffer -> Bool
isValid = undefined
