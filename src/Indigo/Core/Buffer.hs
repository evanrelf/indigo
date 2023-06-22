{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.Buffer
  ( Buffer

    -- * Create
  , fromText
  , fromRope
  , open

    -- * Query
  , path
  , contents
  , selection
  , isModified
  , verticalScroll
  , horizontalScroll

    -- * Modify

    -- * Consume
  )
where

import Data.Default.Class (Default (..))
import Data.Text.Rope (Rope)
import Indigo.Core.Selection (Selection)
import Prelude hiding (empty)

import qualified Data.Text.Rope as Rope

data Buffer = Buffer
  { path :: Maybe FilePath
  , contents :: Rope
  , selection :: Selection
  , isModified :: Bool
  , verticalScroll :: Word
  , horizontalScroll :: Word
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

open :: MonadIO m => FilePath -> m Buffer
open path = do
  contents <- Rope.fromText <$> readFileText path
  pure def
    { path = Just path
    , contents
    }

path :: Buffer -> Maybe FilePath
path b = b.path

contents :: Buffer -> Rope
contents b = b.contents

selection :: Buffer -> Selection
selection b = b.selection

isModified :: Buffer -> Bool
isModified b = b.isModified

verticalScroll :: Buffer -> Word
verticalScroll b = b.verticalScroll

horizontalScroll :: Buffer -> Word
horizontalScroll b = b.horizontalScroll

-- Selection must be valid in the rope
-- Horizontal scroll offset must not exceed length of longest line
-- Vertical scroll offset must not exceed length of buffer
isValid :: Buffer -> Bool
isValid = undefined
