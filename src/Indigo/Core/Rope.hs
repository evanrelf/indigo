{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  , empty
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
  , toText
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
  , chars :: {-# UNPACK #-} !Word
  }

data NodeMeta = NodeMeta
  { chars :: {-# UNPACK #-} !Word
  , newlines :: {-# UNPACK #-} !Word
  }

instance Semigroup NodeMeta where
  (<>) :: NodeMeta -> NodeMeta -> NodeMeta
  (<>) left right =
    NodeMeta
      { chars = left.chars + right.chars
      , newlines = left.newlines + right.newlines
      }

instance Monoid NodeMeta where
  mempty :: NodeMeta
  mempty =
    NodeMeta
      { chars = 0
      , newlines = 0
      }

instance FingerTree.Measured NodeMeta Node where
  measure :: Node -> NodeMeta
  measure node =
    NodeMeta
      { chars = node.chars
      , newlines = unsafeIntToWord (Text.count "\n" node.text)
      }

newtype CharIndex = CharIndex{ unCharIndex :: Word }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

instance Default CharIndex where
  def :: CharIndex
  def = CharIndex 0

newtype LineIndex = LineIndex{ unLineIndex :: Word }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

instance Default LineIndex where
  def :: LineIndex
  def = LineIndex 0

newtype ColumnIndex = ColumnIndex{ unColumnIndex :: Word }
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
  if node.chars == 0
    then fingerTree
    else node <| fingerTree

snoc :: FingerTree NodeMeta Node -> Node -> FingerTree NodeMeta Node
snoc fingerTree node =
  if node.chars == 0
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
      node = Node{ text, chars = unsafeIntToWord (Text.length text) }

    text : texts -> go (fingerTree `snoc` node) texts
      where
      node = Node{ text, chars = maxLength }

null :: Rope -> Bool
null = viaFingerTree FingerTree.null

lengthChars :: Rope -> Word
lengthChars = viaFingerTree $ (.chars) . FingerTree.measure

-- The number of lines is the number of newlines + 1. That means an empty rope
-- is considered to have one line. This matches the behavior of the `ropey` Rust
-- crate.
lengthLines :: Rope -> Word
lengthLines = viaFingerTree $ (+ 1) . (.newlines) . FingerTree.measure

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
  else do
    let go :: Word -> Word -> Text -> CharIndex
        go !lineCount !charCount !text =
          if lineCount < index then
            case Text.uncons text of
              Just ('\n', text') -> go (lineCount + 1) (charCount + 1) text'
              Just (char, text') -> go lineCount (charCount + 1) text'
              Nothing -> CharIndex charCount
          else
            CharIndex charCount

    Just $ go 0 0 (toText rope)

    -- rope
    -- & toText
    -- & Text.foldr
    --     ( \char (lineCount, charCount) -> do
    --         let lineCount' = if char == '\n' then lineCount + 1 else lineCount
    --         let charCount' = charCount + 1
    --         if lineCount < index
    --           then (lineCount', charCount')
    --           else (lineCount, charCount)
    --     )
    --     (0 :: Word, 0 :: Word)
    -- & snd
    -- & CharIndex
    -- & Just

    -- let text = toText rope
    -- let lines = fmap (`Text.snoc` '\n') (Text.split (== '\n') text)
    -- let length = unsafeIntToWord (Text.length (Text.concat (take (unsafeWordToInt index) lines)))
    -- Just (CharIndex length)

line :: HasCallStack => LineIndex -> Rope -> Maybe Rope
line (LineIndex index) rope =
  if index >= lengthLines rope then
    Nothing
  else do
    let text = toText rope
    let lines = Text.lines text
    let line =
          fromMaybe
            (error "line: unreachable")
            (lines !!? unsafeWordToInt index)
    Just (fromText line)

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
