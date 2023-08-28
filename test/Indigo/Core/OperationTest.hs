module Indigo.Core.OperationTest where

import Hedgehog
import Indigo.Core.Operation
import Indigo.Core.Utilities (unsafeWordToInt)

import qualified Hedgehog.Gen as Gen

genOperation :: Range Word -> Gen Operation
genOperation range = do
  Gen.choice
    [ Retain <$> Gen.word range
    , Delete <$> Gen.word range
    , Insert <$> Gen.text (unsafeWordToInt <$> range) Gen.alphaNum
    ]

genOperations :: Gen Operations
genOperations = undefined
