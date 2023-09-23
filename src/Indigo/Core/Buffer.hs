module Indigo.Core.Buffer
  ( Buffer (..)
  , InMemoryBuffer
  , FileBuffer

    -- * Query
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

import Indigo.Core.Buffer.File (FileBuffer)
import Indigo.Core.Buffer.File qualified as FileBuffer
import Indigo.Core.Buffer.InMemory (InMemoryBuffer)
import Indigo.Core.Buffer.InMemory qualified as InMemoryBuffer
import Indigo.Core.Rope (ColumnIndex, LineIndex, Rope)
import Indigo.Core.Selection (Selection)

data Buffer
  = InMemory !InMemoryBuffer
  | File !FileBuffer

contents :: Buffer -> Rope
contents = \case
  InMemory buffer -> InMemoryBuffer.contents buffer
  File buffer -> FileBuffer.contents buffer

selection :: Buffer -> Selection
selection = \case
  InMemory buffer -> InMemoryBuffer.selection buffer
  File buffer -> FileBuffer.selection buffer

isReadOnly :: Buffer -> Bool
isReadOnly = \case
  InMemory buffer -> InMemoryBuffer.isReadOnly buffer
  File buffer -> FileBuffer.isReadOnly buffer

verticalScroll :: Buffer -> LineIndex
verticalScroll = \case
  InMemory buffer -> InMemoryBuffer.verticalScroll buffer
  File buffer -> FileBuffer.verticalScroll buffer

horizontalScroll :: Buffer -> ColumnIndex
horizontalScroll = \case
  InMemory buffer -> InMemoryBuffer.horizontalScroll buffer
  File buffer -> FileBuffer.horizontalScroll buffer

scrollUp :: Word -> Buffer -> Buffer
scrollUp distance = \case
  InMemory buffer -> InMemory $ InMemoryBuffer.scrollUp distance buffer
  File buffer -> File $ FileBuffer.scrollUp distance buffer

scrollDown :: Word -> Buffer -> Buffer
scrollDown distance = \case
  InMemory buffer -> InMemory $ InMemoryBuffer.scrollDown distance buffer
  File buffer -> File $ FileBuffer.scrollDown distance buffer

scrollLeft :: Word -> Buffer -> Buffer
scrollLeft distance = \case
  InMemory buffer -> InMemory $ InMemoryBuffer.scrollLeft distance buffer
  File buffer -> File $ FileBuffer.scrollLeft distance buffer

scrollRight :: Word -> Buffer -> Buffer
scrollRight distance = \case
  InMemory buffer -> InMemory $ InMemoryBuffer.scrollRight distance buffer
  File buffer -> File $ FileBuffer.scrollRight distance buffer

scrollToLine :: LineIndex -> Buffer -> Buffer
scrollToLine line = \case
  InMemory buffer -> InMemory $ InMemoryBuffer.scrollToLine line buffer
  File buffer -> File $ FileBuffer.scrollToLine line buffer

scrollToColumn :: ColumnIndex -> Buffer -> Buffer
scrollToColumn column = \case
  InMemory buffer -> InMemory $ InMemoryBuffer.scrollToColumn column buffer
  File buffer -> File $ FileBuffer.scrollToColumn column buffer

isValid :: Buffer -> Bool
isValid = \case
  InMemory buffer -> InMemoryBuffer.isValid buffer
  File buffer -> FileBuffer.isValid buffer
