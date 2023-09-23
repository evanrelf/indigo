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

import Indigo.Core.Buffer.InMemory.Internal
