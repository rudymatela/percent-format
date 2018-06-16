import Test.Speculate
import Text.PercentFormat

-- TODO: make this work by tweaking the String enumeration
main :: IO ()
main = speculate args
  { maxTests = 100000
  , constants =
      [ constant "%"   ((%)   ->:> int)
      , constant "-%"  ((-%)  ->:> int)
--    , constant "%%"  ((%%)  ->:> (int,int))
--    , constant "-%%" ((-%%) ->:> (int,int))
      , constant "/%"  (/%)
      ]
  }
