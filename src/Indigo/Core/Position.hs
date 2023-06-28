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
          then (True, Rope.length rope |- 1)
          else (False, index0)

  let line = Rope.codePointToLine index rope

  let column = index - Rope.lineToCodePoint line rope

  let position = Position{ line, column }

  if corrected
    then Left position
    else Right position

-- TODO: This is probably horribly incorrect and inefficient
toRopeIndex :: Position -> Rope -> Either Word Word
toRopeIndex position rope = do
  when (Rope.length rope == 0) do
    error "cannot handle empty ropes yet"

  let lines = Rope.lengthInLines rope |- 1

  let (corrected_line, line) =
        if lines <= position.line
          then (True, lines |- 1)
          else (False, position.line)

  let columns =
        case Rope.line line rope of
          Nothing -> error "uh oh"
          Just l -> Rope.length l

  let (corrected_column, column) =
        if columns <= position.column
          then (True, columns |- 1)
          else (False, position.column)

  let corrected = corrected_line || corrected_column

  let index = Rope.lineToCodePoint line rope + column

  if corrected
    then Left index
    else Right index

-- TODO: Move these functions somewhere more appropriate

-- TODO: THIS IS WRONG
-- | Saturating subtraction
(|-) :: (Bounded a, Ord a, Num a) => a -> a -> a
(|-) x y =
  if x == minBound && y >= 0
    then x
    else x - y

infixl 6 |-
