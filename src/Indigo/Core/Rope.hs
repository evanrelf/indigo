{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Indigo.Core.Rope
  ( -- * Extras
    line
  , codePointToLine
  , lineToCodePoint

    -- * Re-exports
  , module Data.Text.Rope
  )
where

import Data.Text.Rope

import qualified Data.Bits as Bits
import qualified Data.Text.Rope as Rope

line :: Word -> Rope -> Maybe Rope
line index0 rope = do
  index <- Bits.toIntegralSized index0
  text <- Rope.lines rope !!? index
  pure $ Rope.fromText text

codePointToLine :: Word -> Rope -> Word
codePointToLine index rope = do
  let (before, _) = Rope.splitAt index rope
  (max (Rope.lengthInLines before) 1) - 1

lineToCodePoint :: Word -> Rope -> Word
lineToCodePoint line rope = do
  let (before, _) = Rope.splitAtLine line rope
  (max (Rope.length before) 1) - 1
