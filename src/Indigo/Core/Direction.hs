module Indigo.Core.Direction
  ( Direction (..)
  )
where

data Direction
  = Forward
  | Backward
  deriving stock (Show, Eq)
