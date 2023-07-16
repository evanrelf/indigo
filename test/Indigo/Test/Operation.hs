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

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

tests :: Group
tests = $$(discover)

genOperation :: Gen (Word -> Operations -> Operations)
genOperation = undefined

genOperations :: Gen Operations
genOperations = undefined
