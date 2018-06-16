-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Text.PercentFormat
import Test.LeanCheck


-- TODO: take the format string as a parameter
maini :: String -> String
maini _ = unlines $ ("%s" %&) `map` take 12 (list :: [String])
                 ++ ("%s" %&) `map` take 12 (list :: [Int])
                 ++ ("%s" %&) `map` take 12 (list :: [Float])
                 ++ ("%s" %&) `map` take 12 (list :: [Rational])

main :: IO ()
main = interact maini

(%&) :: Show a => String -> a -> String
s %& x = "%r %% %12-s == %r" % s % show11 x -% ("%s" % x)

show11 :: Show a => a -> String
show11 x = showsPrec 11 x ""
