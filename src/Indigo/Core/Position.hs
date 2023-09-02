module Indigo.Core.Position
  ( Position (..)

    -- * Create
  , fromCharIndex

    -- * Consume
  , toCharIndex
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

fromCharIndex :: HasCallStack => CharIndex -> Rope -> Conversion Position
fromCharIndex _ rope | Rope.null rope = Invalid
fromCharIndex index0 rope = do
  let (corrected, index) = do
        let lengthChars = Rope.lengthChars rope
        if lengthChars <= unCharIndex index0
          then (True, CharIndex (lengthChars |- 1))
          else (False, index0)

  let line = fromMaybe (error "uh oh") (Rope.charToLine index rope)

  let column = maybe (error "uh oh") (index -) (Rope.lineToChar line rope)

  let position =
        Position
          { line = unLineIndex line
          , column = unCharIndex column
          }

  if corrected
    then Corrected position
    else Valid position

toCharIndex :: HasCallStack => Position -> Rope -> Conversion CharIndex
toCharIndex _ rope | Rope.null rope = Invalid
toCharIndex position rope = do
  let (correctedLine, line) = do
        let lines = Rope.lengthLines rope |- 1
        if lines <= position.line
          -- When line goes beyond end of rope, use last line
          then (True, LineIndex lines)
          else (False, LineIndex position.line)

  let (correctedColumn, column) = do
        let columns = maybe (error "uh oh") Rope.lengthChars (Rope.line line rope)
        if correctedLine then
          -- When line was corrected, use last column
          (True, CharIndex columns)
        else if columns <= position.column then
          -- When column goes beyond end of line, use last column
          (True, CharIndex (columns |- 1))
        else
          (False, CharIndex position.column)

  let corrected = correctedLine || correctedColumn

  let index = maybe (error "uh oh") (column +) (Rope.lineToChar line rope)

  if corrected
    then Corrected index
    else Valid index