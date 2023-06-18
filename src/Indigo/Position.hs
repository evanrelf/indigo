module Indigo.Position
  ( Position (..)

    -- * Create
  , fromIndices

    -- * Consume
  , toIndices
  )
where

data Position = Position
  { line :: Word
  , column :: Word
  }
  deriving stock (Eq, Ord)

fromIndices :: Word -> Word -> Position
fromIndices line column = Position{ line, column }

toIndices :: Position -> (Word, Word)
toIndices position = (position.line, position.column)
