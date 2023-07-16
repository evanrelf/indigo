module Indigo.Core.Operation
  ( Operation (..)
  , invert
  , apply
  )
where

import Indigo.Core.Rope (Rope)

data Operation
  = Retain {-# UNPACK #-} !Word
  | Delete {-# UNPACK #-} !Word
  | Insert !Text
  deriving stock (Show, Eq)

invert :: Operation -> Operation
invert = \case
  Retain count -> undefined
  Delete count -> undefined
  Insert text -> undefined

apply :: Operation -> Rope -> Rope
apply operation rope =
  case operation of
    Retain count -> undefined
    Delete count -> undefined
    Insert text -> undefined
