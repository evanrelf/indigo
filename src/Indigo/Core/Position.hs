module Indigo.Core.Position
  ( Position (..)

    -- * Create
  , fromRopeIndex

    -- * Consume
  , toRopeIndex
  )
where

import Data.Default.Class (Default (..))
import Indigo.Core.Conversion (Conversion (..))
import Indigo.Core.Rope (CharIndex (..), LineIndex (..), Rope)
import Indigo.Core.Utilities ((|-))
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

fromRopeIndex
  :: HasCallStack
  => CharIndex
  -> Rope
  -> Conversion Position
fromRopeIndex _ rope | Rope.null rope = Invalid
fromRopeIndex index0 rope = do
  let lengthChars = Rope.lengthChars rope

  let (corrected, index) =
        if lengthChars <= coerce index0
          then (True, CharIndex (lengthChars |- 1))
          else (False, index0)

  let line = fromMaybe (error "uh oh") (Rope.charToLine index rope)

  let column = maybe (error "uh oh") (index -) (Rope.lineToChar line rope)

  let position =
        Position
          { line = coerce line
          , column = coerce column
          }

  if corrected
    then Corrected position
    else Valid position

toRopeIndex
  :: HasCallStack
  => Position
  -> Rope
  -> Conversion CharIndex
toRopeIndex _ rope | Rope.null rope = Invalid
toRopeIndex position rope = do
  let lines = Rope.lengthLines rope |- 1

  let (corrected_line, line) =
        if lines <= position.line
          then (True, LineIndex (lines |- 1))
          else (False, LineIndex position.line)

  let columns = maybe (error "uh oh") Rope.lengthChars (Rope.line line rope)

  let (corrected_column, column) =
        if columns <= position.column
          then (True, CharIndex (columns |- 1))
          else (False, CharIndex position.column)

  let corrected = corrected_line || corrected_column

  let index = maybe (error "uh oh") (column +) (Rope.lineToChar line rope)

  if corrected
    then Corrected index
    else Valid index
