module Indigo.Core.Conversion
  ( Conversion (..)
  )
where

data Conversion e a
  = Invalid !e
  | Corrected !a
  | Valid !a
  deriving stock (Show, Eq)
