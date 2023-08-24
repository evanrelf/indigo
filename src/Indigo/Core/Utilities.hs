module Indigo.Core.Utilities
  ( (|-)
  , (+|)
  , unsafeIntToWord
  , unsafeWordToInt
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

unsafeIntToWord :: HasCallStack => Int -> Word
unsafeIntToWord int =
  case toIntegralSized int of
    Nothing -> error "unsafeIntToWord: given negative integer"
    Just word -> word

unsafeWordToInt :: HasCallStack => Word -> Int
unsafeWordToInt word =
  case toIntegralSized word of
    Nothing -> error "unsafeWordToInt: given huge word"
    Just int -> int
