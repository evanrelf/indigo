{-# OPTIONS_GHC -Wno-orphans #-}

module Indigo.Core.HistoryTest
  ( module Indigo.Core.HistoryTest
  )
where

import Hedgehog hiding (Action)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Indigo.Core.History
import Prelude hiding (empty, length)
import Relude.Unsafe ((!!))

hprop_window_shopping_doesnt_fork :: Property
hprop_window_shopping_doesnt_fork = property do
  history <- forAll $ genHistory (Range.linear 0 10) $ Gen.int (Range.linear 0 9)
  forward <- do
    count <- forAll $ Gen.int (Range.linear 0 20)
    pure $ iterate (travelForward .) travelForward !! count
  backward <- do
    count <- forAll $ Gen.int (Range.linear 0 20)
    pure $ iterate (travelBackward .) travelBackward !! count
  length history === length (backward (forward history))

hprop_acting_in_the_past_sends_you_to_the_present :: Property
hprop_acting_in_the_past_sends_you_to_the_present = property do
  history1 <- forAll $ genHistory (Range.linear 1 10) $ Gen.int (Range.linear 0 9)
  let history2 = travelBackward history1
  assert $ not (null (future history2))
  let history3 = act 42 history2
  assert $ null (future history3)

instance Action Int where
  invert :: Int -> Int
  invert = negate

genHistory :: Action a => Range Int -> Gen a -> Gen (History a)
genHistory size genAction = do
  actions <- Gen.list size genAction
  pure $ foldl' (flip act) empty actions
