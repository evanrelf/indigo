{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Indigo.Core.Rope
  ( Rope
  , CharIndex (..)
  , LineIndex (..)
  , ColumnIndex (..)

    -- * Create
  -- , empty
  , fromText

    -- * Query
  , null
  , lengthChars
  , lengthLines
  , charToLine
  , lineToChar
  , line

    -- * Modify
  -- , insertChar
  -- , insertText
  -- , remove
  -- , append
  -- , splitAt

    -- * Consume
  -- , toText
  )
where

import Data.Default.Class (Default (..))
import Data.FingerTree (FingerTree, ViewL (..), ViewR (..), (<|), (|>))
import Indigo.Core.Utilities (unsafeIntToWord, unsafeWordToInt)
import Prelude hiding (empty, null, toText, splitAt, lines)

import qualified Data.FingerTree as FingerTree
import qualified Data.Text as Text
import qualified Text.Show

newtype Rope = Rope (FingerTree NodeMeta Node)

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

instance Default Rope where
  def :: Rope
  def = empty

instance Show Rope where
  show :: Rope -> String
  show = Text.Show.show . toText

instance IsString Rope where
  fromString :: String -> Rope
  fromString = fromText . fromString

data Node = Node
  { text :: !Text
  , lengthChars :: {-# UNPACK #-} !Word
  }

data NodeMeta = NodeMeta
  { lengthChars :: {-# UNPACK #-} !Word
  , lengthLines :: {-# UNPACK #-} !Word
  }

instance Semigroup NodeMeta where
  (<>) :: NodeMeta -> NodeMeta -> NodeMeta
  (<>) left right =
    NodeMeta
      { lengthChars = left.lengthChars + right.lengthChars
      , lengthLines = left.lengthLines + right.lengthLines
      }

instance Monoid NodeMeta where
  mempty :: NodeMeta
  mempty =
    NodeMeta
      { lengthChars = 0
      , lengthLines = 0
      }

instance FingerTree.Measured NodeMeta Node where
  measure :: Node -> NodeMeta
  measure node =
    NodeMeta
      { lengthChars = node.lengthChars
      , lengthLines = unsafeIntToWord (Text.count "\n" node.text)
      }

newtype CharIndex = CharIndex Word
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

instance Default CharIndex where
  def :: CharIndex
  def = CharIndex 0

newtype LineIndex = LineIndex Word
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

instance Default LineIndex where
  def :: LineIndex
  def = LineIndex 0

newtype ColumnIndex = ColumnIndex Word
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

instance Default ColumnIndex where
  def :: ColumnIndex
  def = ColumnIndex 0

-- TODO: Determine value experimentally
maxLength :: Integral a => a
maxLength = 1024

cons :: Node -> FingerTree NodeMeta Node -> FingerTree NodeMeta Node
cons node fingerTree =
  if node.lengthChars == 0
    then fingerTree
    else node <| fingerTree

snoc :: FingerTree NodeMeta Node -> Node -> FingerTree NodeMeta Node
snoc fingerTree node =
  if node.lengthChars == 0
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

    text : [] -> fingerTree `snoc` node
      where
      node = Node{ text, lengthChars = unsafeIntToWord (Text.length text) }

    text : texts -> go (fingerTree `snoc` node) texts
      where
      node = Node{ text, lengthChars = maxLength }

null :: Rope -> Bool
null = viaFingerTree FingerTree.null

lengthChars :: Rope -> Word
lengthChars = viaFingerTree $ (.lengthChars) . FingerTree.measure

lengthLines :: Rope -> Word
lengthLines = viaFingerTree $ (.lengthLines) . FingerTree.measure

charToLine :: HasCallStack => CharIndex -> Rope -> Maybe LineIndex
charToLine (CharIndex index) rope =
  if index == 0 then
    Just (LineIndex 0)
  else if index >= lengthChars rope then
    Nothing
  else do
    let text = toText rope
    -- TODO: Use `Rope.splitAt`
    let (before, _after) = Text.splitAt (unsafeWordToInt index) text
    let lines = unsafeIntToWord (Text.count "\n" before)
    Just (LineIndex lines)

-- TODO: In use
lineToChar :: LineIndex -> Rope -> Maybe CharIndex
lineToChar (LineIndex index) rope =
  if index >= lengthLines rope then
    Nothing
  else
    undefined

-- TODO: In use
line :: LineIndex -> Rope -> Maybe Rope
line (LineIndex index) rope =
  if index >= lengthLines rope then
    Nothing
  else
    undefined

insertChar :: CharIndex -> Char -> Rope -> Rope
insertChar = undefined

insertText :: CharIndex -> Text -> Rope -> Rope
insertText = undefined

remove :: CharIndex -> CharIndex -> Rope -> Rope
remove = undefined

-- TODO: Merge smaller nodes at connecting ends?
append :: Rope -> Rope -> Rope
append = viaFingerTree (<>)

-- splitAt :: Int -> YiString -> (YiString, YiString)
-- splitAt n (YiString t)
--   | n <= 0 = (mempty, YiString t)
--   | otherwise = case viewl s of
--     Chunk l x :< ts | n' /= 0 ->
--       let (lx, rx) = TX.splitAt n' x
--       in (YiString $ f |> Chunk n' lx,
--           YiString $ Chunk (l - n') rx -| ts)
--     _ -> (YiString f, YiString s)
--   where
--     (f, s) = T.split ((> n) . charIndex) t
--     n' = n - charIndex (measure f)

splitAt :: CharIndex -> Rope -> (Rope, Rope)
splitAt (CharIndex index) rope | index <= 0 = (empty, rope)
splitAt (CharIndex index) (Rope fingerTree) =
  case FingerTree.viewl fingerTree of
    FingerTree.EmptyL -> (empty, empty)
    node :< nodes -> undefined

      -- | n' == 0 -> (Rope front, Rope back)
      -- | otherwise -> undefined
      -- where
      -- (front, back) = FingerTree.split ((> index) . (.lengthChars)) fingerTree
      -- index' = index - (FingerTree.measure front).lengthChars

toText :: Rope -> Text
toText = viaFingerTree $ Text.concat . go []
  where
  go :: [Text] -> FingerTree NodeMeta Node -> [Text]
  go texts fingerTree =
    case FingerTree.viewr fingerTree of
      FingerTree.EmptyR -> texts
      nodes :> node -> go (node.text : texts) nodes
