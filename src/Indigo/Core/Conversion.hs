module Indigo.Core.Conversion
  ( Conversion
  , Conversion' (..)
  )
where

type Conversion a = Conversion' a a

data Conversion' e a
  = Invalid
  | Corrected e
  | Valid a
  deriving stock (Show, Eq)

instance Functor (Conversion' e) where
  fmap :: (a -> b) -> Conversion' e a -> Conversion' e b
  fmap f = \case
    Invalid -> Invalid
    Corrected x -> Corrected x
    Valid x -> Valid (f x)

instance Bifunctor Conversion' where
  bimap :: (a -> b) -> (c -> d) -> Conversion' a c -> Conversion' b d
  bimap _ _ Invalid = Invalid
  bimap f _ (Corrected x) = Corrected (f x)
  bimap _ f (Valid x) = Valid (f x)

instance Applicative (Conversion' e) where
  pure :: a -> Conversion' e a
  pure x = Valid x

  (<*>) :: Conversion' e (a -> b) -> Conversion' e a -> Conversion' e b
  (<*>) Invalid _ = Invalid
  (<*>) _ Invalid = Invalid
  (<*>) (Corrected x) _ = Corrected x
  (<*>) (Valid f) x = fmap f x

instance Alternative (Conversion' e) where
  empty :: Conversion' e a
  empty = Invalid

  (<|>) :: Conversion' e a -> Conversion' e a -> Conversion' e a
  (<|>) Invalid x = x
  (<|>) x Invalid = x
  (<|>) (Corrected x) _ = Corrected x
  (<|>) (Valid x) _ = Valid x

instance Monad (Conversion' e) where
  (>>=) :: Conversion' e a -> (a -> Conversion' e b) -> Conversion' e b
  (>>=) (Valid x) k = k x
  (>>=) Invalid _ = Invalid
  (>>=) (Corrected x) _ = Corrected x
