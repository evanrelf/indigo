module Indigo.Position
  ( Position (..)

    -- * Create
  , fromIndices

    -- * Consume
  , toIndices
  )
where

import Data.Default.Class (Default (..))

data Position = Position
  { line :: Word
  , column :: Word
  }
  deriving stock (Eq, Ord)

instance Default Position where
  def :: Position
  def = Position{ line = 0, column = 0 }

fromIndices :: Word -> Word -> Position
fromIndices line column = Position{ line, column }

toIndices :: Position -> (Word, Word)
toIndices position = (position.line, position.column)
