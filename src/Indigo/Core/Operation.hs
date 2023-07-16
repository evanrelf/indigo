{-# LANGUAGE NoFieldSelectors #-}

module Indigo.Core.Operation
  ( Operations
  , Affinity (..)
  , retain
  , delete
  , insert
  , compose
  , invert
  , apply
  )
where

import Data.Default.Class (Default (..))
import Data.Sequence (Seq (..))
import Indigo.Core.Rope (Rope)
import Indigo.Core.Selection (Selection)
import Indigo.Core.Utilities (unsafeIntToWord)

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Indigo.Core.Rope as Rope

data Operations = Operations
  { operations :: !(Seq Operation)
  , lengthBefore :: {-# UNPACK #-} !Word
  , lengthAfter :: {-# UNPACK #-} !Word
  }
  deriving stock (Show, Eq)

instance Default Operations where
  def :: Operations
  def =
    Operations
      { operations = Seq.empty
      , lengthBefore = 0
      , lengthAfter = 0
      }

data Operation
  = Retain {-# UNPACK #-} !Word
  | Delete {-# UNPACK #-} !Word
  | Insert !Text
  deriving stock (Show, Eq)

data Affinity
  = Before
  | After
  deriving stock (Show, Eq)

retain :: Word -> Operations -> Operations
retain 0 operations = operations
retain count operations =
  operations
    { operations = operations.operations `absorb` Retain count
    , lengthBefore = operations.lengthBefore + count
    , lengthAfter = operations.lengthAfter + count
    }

delete :: Word -> Operations -> Operations
delete 0 operations = operations
delete count operations =
  operations
    { operations = operations.operations `absorb` Delete count
    , lengthBefore = operations.lengthBefore + count
    }

insert :: Text -> Operations -> Operations
insert text operations =
  operations
    { operations = operations.operations `absorb` Insert text
    , lengthAfter = operations.lengthAfter + unsafeIntToWord (Text.length text)
    }

absorb :: Seq Operation -> Operation -> Seq Operation
absorb Empty operation = Seq.singleton operation
absorb (operations :|> previousOperation) nextOperation =
  case (previousOperation, nextOperation) of
    (Retain previous, Retain next) -> operations :|> Retain (previous + next)
    (Delete previous, Delete next) -> operations :|> Delete (previous + next)
    (Insert previous, Insert next) -> operations :|> Insert (previous <> next)
    _ -> operations :|> previousOperation :|> nextOperation

compose :: Operations -> Operations -> Maybe Operations
compose = undefined

invert :: Rope -> Operations -> Operations
invert = undefined

apply :: Rope -> Operations -> Maybe Rope
apply = undefined
