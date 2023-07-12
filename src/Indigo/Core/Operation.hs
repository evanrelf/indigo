module Indigo.Core.Operation
  ( Operation (..)
  , InsertOperation (..)
  , DeleteOperation (..)
  , invert
  , apply
  )
where

import Indigo.Core.Rope (CharIndex, Rope)

data Operation
  = Insert !InsertOperation
  | Delete !DeleteOperation
  deriving stock (Show, Eq)

data InsertOperation = InsertOperation
  { index :: {-# UNPACK #-} !CharIndex
  , text :: !Text
  }
  deriving stock (Show, Eq)

data DeleteOperation = DeleteOperation
  { index :: {-# UNPACK #-} !CharIndex
  , text :: !Text
  }
  deriving stock (Show, Eq)

invert :: Operation -> Operation
invert = \case
  Insert InsertOperation{index, text} -> Delete DeleteOperation{index, text}
  Delete DeleteOperation{index, text} -> Insert InsertOperation{index, text}

apply :: Operation -> Rope -> Rope
apply operation rope =
  case operation of
    Insert InsertOperation{index, text} -> undefined
    Delete DeleteOperation{index, text} -> undefined
