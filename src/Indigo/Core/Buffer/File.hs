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

import Indigo.Core.Buffer.File.Internal
