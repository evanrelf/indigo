{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
  , toFileBuffer

    -- * Internal
  , isValid
  )
where

import Indigo.Core.Buffer.File.Internal (FileBuffer (..))
import Indigo.Core.Buffer.InMemory.Internal

toFileBuffer :: FilePath -> InMemoryBuffer -> FileBuffer
toFileBuffer path InMemoryBuffer{..} =
  FileBuffer
    { path
    , isModified = False
    , ..
    }
