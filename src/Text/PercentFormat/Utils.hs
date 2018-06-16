module Text.PercentFormat.Utils
  ( maybeRead
  , align
  , rightAlign
  , leftAlign
  , showWithBase
  , applyWhen
  , intsToDigits
  , theLast
  , loop
  )
where

import Data.Maybe (listToMaybe)
import Data.List (unfoldr)
import Data.Tuple (swap)
import Data.Char (intToDigit)

-- | Reads a value encoded as a string,
--   return 'Just' the value or 'Nothing' on error.
maybeRead :: Read a => String -> Maybe a
-- TODO: Use readsPrec to implement this.
maybeRead = listToMaybe . map fst . reads

align :: Bool -> Char -> Int -> String -> String
align left = if left
               then leftAlign
               else rightAlign

-- | @rightAlign c w s@ aligns 'String' @s@ to the right
--   in a field of width @w@ using 'Char' @c@ as padding.
--
-- > right ' ' 5 "123"
-- "  123"
rightAlign :: Char -> Int -> String -> String
rightAlign c width s | width <= len = s
                     | otherwise    = replicate (width - len) c ++ s
  where
  len = length s

-- | @left c w s@ aligns 'String' @s@ to the left
--   in a field of width @w@ using 'Char' @c@ as padding.
--
-- > left ' ' 5 "123"
-- "123  "
leftAlign :: Char -> Int -> String -> String
leftAlign c width s | width <= len = s
                    | otherwise    = s ++ replicate (width - len) c
  where
  len = length s

-- | @showWithBase b n@ returns a string representation of @n@ in base @b@.
--
-- > showWithBase 2 10
-- "1010"
-- > showWithBase 16 49406
-- "c0f3"
-- > showWithBase 10 (-1234)
-- "-1234"
showWithBase :: Integral a => Int -> a -> String
showWithBase b 0 = "0"
showWithBase b n | n < 0     = '-':showWithBase b (abs n)
                 | otherwise = map (intToDigit . fromIntegral)
                             . reverse
                             . unfoldr (\n -> listToMaybe [swap $ n `divMod` fromIntegral b | n /= 0])
                             $ n


applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f x = f x
applyWhen False f x = x

intsToDigits :: [Int] -> String
intsToDigits = map intToDigit

theLast :: Int -> [a] -> [a]
theLast n xs = drop (length xs - n) xs

-- | Like cycle, but return an empty list when the source list is empty.
loop :: [a] -> [a]
loop [] = []
loop xs = cycle xs
