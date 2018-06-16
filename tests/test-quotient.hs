import Test

import Test.LeanCheck
import Test.LeanCheck.Utils

import Text.PercentFormat.Quotient

main :: IO ()
main = reportTests (tests 360)

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ commutative ((+) -:> quotient)
  , holds n $ associative ((+) -:> quotient)
  , holds n $ commutative ((*) -:> quotient)
  , holds n $ associative ((*) -:> quotient)
  , holds n $ distributive (*) ((+) -:> quotient)
  , holds n $ idempotent (+0) -:> quotient
  , holds n $ idempotent (*1) -:> quotient
  , holds n $ idempotent abs -:> quotient
  , holds n $ idempotent signum -:> quotient
  , holds n $ \x -> negate (negate x) == (x -: quotient)
  , holds n $ \x -> abs x * signum x == (x -: quotient)

  , readQ "Infinity" == infinity
  , readQ "-Infinity" == -infinity
  , readQ "NaN" == nan
  , maybeReadQ "blah" == Nothing
  , readQ "10" == 10
  , readQ "-300" == (-300)
  , readQ "10.10" == 101 % 10
  , readQ "10.1" == 101 % 10
  , readQ "33.3e2" == 3330
  , readQ "3.33e2" == 333
  , round (1/2 :: Quotient) == 0
  , round (3/2 :: Quotient) == 2
  ]
