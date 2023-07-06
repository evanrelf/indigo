{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.FingerTree (FingerTree, (<|), (|>), ViewR (..))

import qualified Data.FingerTree as FingerTree
import qualified Data.Text as Text
import qualified Text.Show

newtype Rope = Rope{ unRope :: FingerTree NodeMeta Node }

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

data Node = Node
  { text :: Text
  , meta :: NodeMeta
  }
  deriving stock (Eq)

data NodeMeta = NodeMeta
  { length :: Word
  }
  deriving stock (Eq)

instance Semigroup NodeMeta where
  (<>) :: NodeMeta -> NodeMeta -> NodeMeta
  (<>) left right =
    NodeMeta
      { length = left.length + right.length
      }

instance Monoid NodeMeta where
  mempty :: NodeMeta
  mempty =
    NodeMeta
      { length = 0
      }

instance FingerTree.Measured NodeMeta Node where
  measure :: Node -> NodeMeta
  measure chunk = chunk.meta

-- TODO: Determine value experimentally
maxLength :: Integral a => a
maxLength = 1024

(-|) :: Node -> FingerTree NodeMeta Node -> FingerTree NodeMeta Node
(-|) node fingerTree =
  if node.meta.length == 0
    then fingerTree
    else node <| fingerTree

(|-) :: FingerTree NodeMeta Node -> Node -> FingerTree NodeMeta Node
(|-) fingerTree node =
  if node.meta.length == 0
    then fingerTree
    else fingerTree |> node

class Coercible i o => ViaFingerTree i o | o -> i
instance {-# OVERLAPPABLE #-} i ~ o => ViaFingerTree i o
instance i ~ FingerTree NodeMeta Node => ViaFingerTree i Rope
instance (i ~ FingerTree NodeMeta Node, ViaFingerTree a b) => ViaFingerTree (i -> a) (Rope -> b)

viaFingerTree :: ViaFingerTree i o => i -> o
viaFingerTree = coerce

empty :: Rope
empty = viaFingerTree FingerTree.empty

fromText :: Text -> Rope
fromText = viaFingerTree . go FingerTree.empty . Text.chunksOf maxLength
  where
  go :: FingerTree NodeMeta Node -> [Text] -> FingerTree NodeMeta Node
  go fingerTree = \case
    [] -> fingerTree

    text : [] -> fingerTree |- node
      where
      node = Node{ text, meta }
      meta = NodeMeta{ length = intToWord (Text.length text) }

    text : texts -> go (fingerTree |- node) texts
      where
      node = Node{ text, meta }
      meta = NodeMeta{ length = maxLength }

  intToWord :: Int -> Word
  intToWord = fromMaybe (error "unreachable") . toIntegralSized

null :: Rope -> Bool
null = viaFingerTree FingerTree.null

lengthChars :: Rope -> Word
lengthChars = viaFingerTree $ (.length) . FingerTree.measure

lengthLines :: Rope -> Word
lengthLines = undefined

insertChar :: Word -> Char -> Rope -> Rope
insertChar = undefined

insertText :: Word -> Text -> Rope -> Rope
insertText = undefined

remove :: Word -> Word -> Rope -> Rope
remove = undefined

-- TODO: Merge smaller nodes at connecting ends?
append :: Rope -> Rope -> Rope
append = viaFingerTree (<>)

splitAt :: Word -> Rope -> (Rope, Rope)
splitAt = undefined

toText :: Rope -> Text
toText = Text.concat . go [] . unRope
  where
  go :: [Text] -> FingerTree NodeMeta Node -> [Text]
  go texts fingerTree =
    case FingerTree.viewr fingerTree of
      FingerTree.EmptyR -> texts
      nodes :> Node{ text } -> go (text : texts) nodes
