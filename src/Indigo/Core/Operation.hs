{-# LANGUAGE NoFieldSelectors #-}

module Indigo.Core.Operation
  ( Operations
  , Operation (..)
  , Affinity (..)
  , retain
  , delete
  , insert
  , compose
  , invert
  , apply
  )
where

import Control.Monad.ST qualified as ST
import Data.Default.Class (Default (..))
import Data.STRef qualified as ST
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import Indigo.Core.Rope (Rope)
import Indigo.Core.Rope qualified as Rope
import Indigo.Core.Selection (Selection)
import Indigo.Core.Utilities (unsafeIntToWord)

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

-- // Must be called on original rope, before these operations were applied
-- #[must_use]
-- pub fn invert(&self, rope: &Rope) -> Option<Self> {
--     if rope.len_chars() != self.length_before {
--         return None;
--     }

--     let mut operations = Self::default();
--     let mut position = 0;

--     for operation in &self.operations {
--         match operation {
--             Operation::Retain(n) => {
--                 operations.retain(*n);
--                 position += n;
--             }
--             Operation::Delete(n) => {
--                 let s = Cow::from(rope.slice(position..position + n));
--                 operations.insert(s.as_ref());
--                 position += n;
--             }
--             Operation::Insert(s) => {
--                 operations.delete(s.chars().count());
--             }
--         }
--     }

--     Some(operations)
-- }

invert :: Rope -> Operations -> Maybe Operations
invert = undefined

-- invert rope operations | Rope.lengthChars rope /= operations.lengthBefore = Nothing
-- invert rope operations = Just $ ST.runST do
--   operationsRef <- ST.newSTRef (def :: Operations)

--   positionRef <- ST.newSTRef (0 :: Word)

--   for_ operations.operations \case
--     Retain count -> do
--       ST.modifySTRef operationsRef (retain count)
--       ST.modifySTRef positionRef (+ count)

--     Delete count -> do
--       ST.modifySTRef operationsRef (insert undefined)
--       ST.modifySTRef positionRef (+ count)

--     Insert text -> do
--       ST.modifySTRef operationsRef (delete (unsafeIntToWord (Text.length text)))

--   ST.readSTRef operationsRef

-- invert rope operations = do
--   when (Rope.lengthChars rope /= operations.lengthBefore) Nothing

--   let f (os, position) = \case
--         Retain count -> (retain count os, position + count)
--         Delete count -> (insert undefined os, position + count)
--         Insert text -> (delete (unsafeIntToWord (Text.length text)) os, position)

--   Just $ fst $ foldl' f (def, 0) operations.operations

apply :: Rope -> Operations -> Maybe Rope
apply = undefined
