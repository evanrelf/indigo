module Indigo.Core.Position
  ( Position (..)

    -- * Create
  , fromRawParts
  , fromRopeIndex

    -- * Consume
  , toRawParts
  , toRopeIndex
  )
where

import Data.Default.Class (Default (..))
import Indigo.Core.Rope (Rope)
import Prelude hiding (lines)

import qualified Indigo.Core.Rope as Rope

data Position = Position
  { line :: Word
  , column :: Word
  }
  deriving stock (Show, Eq, Ord)

instance Default Position where
  def :: Position
  def = Position{ line = 0, column = 0 }

fromRawParts :: Word -> Word -> Position
fromRawParts line column = Position{ line, column }

toRawParts :: Position -> (Word, Word)
toRawParts position = (position.line, position.column)

-- TODO: This is probably horribly incorrect and inefficient
fromRopeIndex :: Word -> Rope -> Either Position Position
fromRopeIndex index0 rope = do
  when (Rope.length rope == 0) do
    error "cannot handle empty ropes yet"

  let (corrected, index) =
        if Rope.length rope <= index0
          then (True, (max (Rope.length rope) 1) - 1)
          else (False, index0)

  let line = Rope.codePointToLine index rope

  let column = index - Rope.lineToCodePoint line rope

  traceM $ "corrected: " <> show corrected <> ", index: " <> show index <> ", ltcp: " <> show (Rope.lineToCodePoint line rope) <> ", rope: " <> show rope

  let position = Position{ line, column }

  if corrected
    then Left position
    else Right position

-- TODO: This is probably horribly incorrect and inefficient
toRopeIndex :: Position -> Rope -> Either Word Word
toRopeIndex position rope = do
  when (Rope.length rope == 0) do
    error "cannot handle empty ropes yet"

  let lines = (max (Rope.lengthInLines rope) 1) - 1

  let (corrected_line, line) =
        if lines <= position.line
          then (True, (max lines 1) - 1)
          else (False, position.line)

  let columns =
        case Rope.line line rope of
          Nothing -> error "uh oh"
          Just l -> Rope.length l

  let (corrected_column, column) =
        if columns <= position.column
          then (True, (max columns 1) - 1)
          else (False, position.column)

  let corrected = corrected_line || corrected_column

  let index = Rope.lineToCodePoint line rope + column

  if corrected
    then Left index
    else Right index
