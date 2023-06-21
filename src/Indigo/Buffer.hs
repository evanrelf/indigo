{-# LANGUAGE NoFieldSelectors #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Buffer
  ( Buffer

    -- * Create
  , fromRope
  , open

    -- * Query
  , path
  , contents
  , selections
  , isModified
  , verticalScroll
  , horizontalScroll

    -- * Modify

    -- * Consume
  )
where

import Data.Default.Class (Default (..))
import Data.Text.Rope (Rope)
import Prelude hiding (empty)

import qualified Data.Text.Rope as Rope

data Buffer = Buffer
  { path :: Maybe FilePath
  , contents :: Rope
  , selections :: Undefined
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
      , selections = undefined
      , isModified = False
      , verticalScroll = 0
      , horizontalScroll = 0
      }

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

selections :: Buffer -> Undefined
selections buffer = buffer.selections

isModified :: Buffer -> Bool
isModified buffer = buffer.isModified

verticalScroll :: Buffer -> Word
verticalScroll buffer = buffer.verticalScroll

horizontalScroll :: Buffer -> Word
horizontalScroll buffer = buffer.horizontalScroll

-- Selections must be valid in the rope
-- Horizontal scroll offset must not exceed length of longest line
-- Vertical scroll offset must not exceed length of buffer
isValid :: Buffer -> Bool
isValid = undefined
