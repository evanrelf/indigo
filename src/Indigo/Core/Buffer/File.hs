{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
  , toInMemoryBuffer

    -- * Internal
  , isValid
  )
where

import Indigo.Core.Buffer.File.Internal
import Indigo.Core.Buffer.InMemory (Name)
import Indigo.Core.Buffer.InMemory.Internal (InMemoryBuffer (..))

toInMemoryBuffer :: Name -> FileBuffer -> InMemoryBuffer
toInMemoryBuffer name FileBuffer{..} =
  InMemoryBuffer
    { name
    , ..
    }
