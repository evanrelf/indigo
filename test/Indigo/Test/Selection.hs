{-# LANGUAGE TemplateHaskell #-}

module Indigo.Test.Selection where

import Hedgehog hiding (Range)
import Indigo.Selection
import Indigo.Test.Range (genRange)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: Group
tests = $$(discover)

genSelection :: Gen Selection
genSelection = do
  undefined
