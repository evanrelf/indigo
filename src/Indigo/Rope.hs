{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Indigo.Rope
  ( Rope

    -- * Create
  , empty
  , fromText

    -- * Query
  , null
  , lengthChars
  , lengthLines

    -- * Modify
  , insertChar
  , insertText
  , remove
  , append
  , splitAt

    -- * Consume
  , toText
  )
where

import Prelude hiding (empty, null, toText, splitAt)
import Data.FingerTree (FingerTree)

import qualified Data.FingerTree as FingerTree
import qualified Text.Show

newtype Rope = Rope (FingerTree ChunkMeta Chunk)

instance Eq Rope where
  (==) = (==) `on` toText

instance Ord Rope where
  compare :: Rope -> Rope -> Ordering
  compare = compare `on` toText

instance Semigroup Rope where
  (<>) :: Rope -> Rope -> Rope
  (<>) = append

instance Monoid Rope where
  mempty :: Rope
  mempty = empty

instance Show Rope where
  show :: Rope -> String
  show = Text.Show.show . toText

instance IsString Rope where
  fromString :: String -> Rope
  fromString = fromText . fromString

data Chunk = Chunk
  { text :: Text
  , meta :: ChunkMeta
  }
  deriving stock (Eq)

data ChunkMeta = ChunkMeta
  { length :: Word
  }
  deriving stock (Eq)

instance Semigroup ChunkMeta where
  (<>) :: ChunkMeta -> ChunkMeta -> ChunkMeta
  (<>) left right =
    ChunkMeta
      { length = left.length + right.length
      }

instance Monoid ChunkMeta where
  mempty :: ChunkMeta
  mempty =
    ChunkMeta
      { length = 0
      }

instance FingerTree.Measured ChunkMeta Chunk where
  measure :: Chunk -> ChunkMeta
  measure chunk = chunk.meta

empty :: Rope
empty = Rope FingerTree.empty

fromText :: Text -> Rope
fromText = undefined

null :: Rope -> Bool
null (Rope fingerTree) = FingerTree.null fingerTree

lengthChars :: Rope -> Word
lengthChars = undefined

lengthLines :: Rope -> Word
lengthLines = undefined

insertChar :: Word -> Char -> Rope -> Rope
insertChar = undefined

insertText :: Word -> Text -> Rope -> Rope
insertText = undefined

remove :: Word -> Word -> Rope -> Rope
remove = undefined

append :: Rope -> Rope -> Rope
append = undefined

splitAt :: Word -> Rope -> (Rope, Rope)
splitAt = undefined

toText :: Rope -> Text
toText = undefined
