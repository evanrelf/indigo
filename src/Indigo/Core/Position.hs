{-# LANGUAGE BangPatterns #-}

module Indigo.Core.Position
  ( Position (..)

    -- * Create
  , fromRopeIndex

    -- * Consume
  , toRopeIndex
  )
where

import Data.Default.Class (Default (..))
import Indigo.Core.Rope (Rope)
import Prelude hiding (lines)

import qualified Indigo.Core.Rope as Rope

data Position = Position
  { line :: {-# UNPACK #-} !Word
  , column :: {-# UNPACK #-} !Word
  }
  deriving stock (Show, Eq, Ord)

instance Default Position where
  def :: Position
  def = Position{ line = 0, column = 0 }

fromRawParts :: Word -> Word -> Position
fromRawParts line column = Position{ line, column }

toRawParts :: Position -> (Word, Word)
toRawParts position = (position.line, position.column)

fromRopeIndex
  :: HasCallStack
  => Rope.CharIndex
  -> Rope
  -> Either Position Position
fromRopeIndex index0 rope = do
  when (Rope.null rope) do
    -- TODO
    error "cannot handle empty ropes yet"
    Left def

  let lengthChars = Rope.lengthChars rope

  let (corrected, index) =
        if lengthChars <= coerce index0
          then (True, Rope.CharIndex (lengthChars |- 1))
          else (False, index0)

  let line = fromMaybe (error "uh oh") (Rope.charToLine index rope)

  let column = maybe (error "uh oh") (index -) (Rope.lineToChar line rope)

  let position =
        Position
          { line = coerce line
          , column = coerce column
          }

  if corrected
    then Left position
    else Right position

toRopeIndex
  :: HasCallStack
  => Position
  -> Rope
  -> Either Rope.CharIndex Rope.CharIndex
toRopeIndex position rope = do
  when (Rope.null rope) do
    -- TODO
    error "cannot handle empty ropes yet"
    Left def

  let lines = Rope.lengthLines rope |- 1

  let (corrected_line, line) =
        if lines <= position.line
          then (True, Rope.LineIndex (lines |- 1))
          else (False, Rope.LineIndex position.line)

  let columns = maybe (error "uh oh") Rope.lengthChars (Rope.line line rope)

  let (corrected_column, column) =
        if columns <= position.column
          then (True, Rope.CharIndex (columns |- 1))
          else (False, Rope.CharIndex position.column)

  let corrected = corrected_line || corrected_column

  let index = maybe (error "uh oh") (column +) (Rope.lineToChar line rope)

  if corrected
    then Left index
    else Right index

(|-) :: Word -> Word -> Word
(|-) !x !y
  | x >= y = x - y
  | otherwise = minBound

infixl 6 |-

(+|) :: Word -> Word -> Word
(+|) !x !y
  | result < min x y = maxBound
  | otherwise = result
  where result = x + y

infixl 6 +|
