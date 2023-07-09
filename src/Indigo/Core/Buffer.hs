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
import Indigo.Core.Rope (Rope)
import Indigo.Core.Selection (Selection)
import Prelude hiding (empty)

import qualified Indigo.Core.Rope as Rope

data Buffer = Buffer
  { path :: !(Maybe FilePath)
  , contents :: Rope
  , selection :: Selection
  , isModified :: !Bool
  , verticalScroll :: {-# UNPACK #-} !Word
  , horizontalScroll :: {-# UNPACK #-} !Word
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
path buffer = buffer.path

contents :: Buffer -> Rope
contents buffer = buffer.contents

selection :: Buffer -> Selection
selection buffer = buffer.selection

isModified :: Buffer -> Bool
isModified buffer = buffer.isModified

verticalScroll :: Buffer -> Word
verticalScroll buffer = buffer.verticalScroll

horizontalScroll :: Buffer -> Word
horizontalScroll buffer = buffer.horizontalScroll

-- Selection must be valid in the rope
-- Horizontal scroll offset must not exceed length of longest line
-- Vertical scroll offset must not exceed length of buffer
isValid :: Buffer -> Bool
isValid = undefined
