module Indigo.Core.Position
  ( Position (..)

    -- * Create
  , fromRawParts

    -- * Consume
  , toRawParts
  )
where

import Data.Default.Class (Default (..))

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
toRawParts p = (p.line, p.column)
