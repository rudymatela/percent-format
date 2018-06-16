module Test where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (elemIndices)

import Test.LeanCheck

import Text.PercentFormat
import Text.PercentFormat.Quotient as Q

reportTests :: [Bool] -> IO ()
reportTests tests =
  case elemIndices False tests of
    [] -> putStrLn "+++ Tests passed!"
    is -> do putStrLn ("*** Failed tests:" ++ show is)
             exitFailure

quotient :: Quotient
quotient  =  error "dummy quotient value"

instance Listable Quotient where
  tiers  =  mapT (uncurry (Q.%)) . reset
         $  tiers `suchThat` canonical
    where
    --canonical (0,0)  =  True
    --canonical (1,0)  =  True
    canonical (n,d)  =  d > 0 && n `gcd` d == 1
