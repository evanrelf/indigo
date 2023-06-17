module Indigo.Position
  ( Position (..)
  )
where

data Position = Position
  { line :: Word
  , column :: Word
  }
  deriving stock (Eq, Ord)
