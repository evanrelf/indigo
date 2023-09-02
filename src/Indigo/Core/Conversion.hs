module Indigo.Core.Conversion
  ( Conversion (..)
  )
where

data Conversion a
  = Invalid
  | Corrected !a
  | Valid !a
  deriving stock (Show, Eq)
