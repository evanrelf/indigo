module Indigo.Core.Utilities
  ( (|-)
  , (+|)
  )
where

-- | Saturating subtraction
(|-) :: Word -> Word -> Word
(|-) !x !y =
  if x >= y
    then x - y
    else minBound

infixl 6 |-

-- | Saturating addition
(+|) :: Word -> Word -> Word
(+|) !x !y =
  let result = x + y in
  if result < min x y
    then maxBound
    else result

infixl 6 +|
