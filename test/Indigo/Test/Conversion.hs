{-# LANGUAGE TemplateHaskell #-}

module Indigo.Test.Conversion (tests) where

import Hedgehog
import Indigo.Core.Conversion

import qualified Hedgehog.Gen as Gen

tests :: Group
tests = $$(discover)

-- | `fmap id == id`
prop_functor_identity :: Property
prop_functor_identity = property do
  x <- forAll genConversion
  fmap id x === id x

-- | `fmap (f . g) == fmap f . fmap g`
prop_functor_composition :: Property
prop_functor_composition = property do
  let f = id
  let g = id
  x <- forAll genConversion
  fmap (f . g) x === (fmap f . fmap g) x

-- | `pure id <*> v = v`
prop_applicative_identity :: Property
prop_applicative_identity = property do
  v <- forAll genConversion
  (pure id <*> v) === v

-- | `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
prop_applicative_composition :: Property
prop_applicative_composition = property do
  u0 <- forAll genConversion
  v0 <- forAll genConversion
  w <- forAll genConversion
  let u = u0 $> id
  let v = v0 $> id
  (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

-- | `pure f <*> pure x = pure (f x)`
prop_applicative_homomorphism :: Property
prop_applicative_homomorphism = property do
  let f = id
  let x = ()
  (pure @(Conversion' ()) f <*> pure x) === pure (f x)

-- | `u <*> pure y = pure ($ y) <*> u`
prop_applicative_interchange :: Property
prop_applicative_interchange = property do
  u0 <- forAll genConversion
  let u = u0 $> id
  let y = ()
  (u <*> pure y) === (pure ($ y) <*> u)

-- | `return a >>= k = k a`
prop_monad_left_identity :: Property
prop_monad_left_identity = property do
  let a = ()
  k0 <- forAll genConversion
  let k = \_ -> k0
  (return a >>= k) === k a

-- | `m >>= return = m`
prop_monad_right_identity :: Property
prop_monad_right_identity = property do
  m <- forAll genConversion
  (m >>= return) === m

-- | `m >>= (\x -> k x >>= h) = (m >>= k) >>= h`
prop_monad_associativity :: Property
prop_monad_associativity = property do
  m <- forAll genConversion
  k0 <- forAll genConversion
  let k = \_ -> k0
  h0 <- forAll genConversion
  let h = \_ -> h0
  (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)

genConversion :: Gen (Conversion ())
genConversion = do
  Gen.element
    [ Invalid
    , Corrected ()
    , Valid ()
    ]
