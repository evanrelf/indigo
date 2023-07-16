{-# LANGUAGE TemplateHaskell #-}

module Indigo.Test.Operation
  ( tests
  , genOperation
  , genOperations
  )
where

import Hedgehog
import Indigo.Core.Operation
import Indigo.Core.Rope (Rope)
import Indigo.Core.Utilities (unsafeIntToWord, unsafeWordToInt)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: Group
tests = $$(discover)

genOperation :: Range Word -> Gen Operation
genOperation range = do
  Gen.choice
    [ Retain <$> Gen.word range
    , Delete <$> Gen.word range
    , Insert <$> Gen.text (unsafeWordToInt <$> range) Gen.alphaNum
    ]

genOperations :: Gen Operations
genOperations = undefined
