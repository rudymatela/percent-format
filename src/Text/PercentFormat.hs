-- |
-- Module      : Text.PercentFormat
-- Copyright   : (c) 2016-2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- The 'Text.PercentFormat' library provides printf-style string formatting.
-- It provides a '%' operator (as in Ruby or Python)
-- and uses the old C-printf-style format you know and love.
--
-- This library differs from 'Text.Printf' in that it does not rely on custom
-- typeclasses -- it works on anything that is a 'Show' instance.
--
--
-- Formatting one value:
--
-- > > "Hello %s!" -% "World"
-- > "Hello World!"
--
--
-- Formatting three values, tuple style:
--
-- > > "load average: %1.2f %1.2f %1.2f" -%%% (0.00, 0.066, 0.11)
-- > "load average: 0.00 0.07 0.11"
--
--
-- Formatting three values, chain style:
--
-- > > "load average: %1.2f %1.2f %1.2f" % 0.00 % 0.066 -% 0.11
-- > "load average: 0.00 0.07 0.11"
--
--
-- To produce a string with a percent sign (@%@),
-- use two percent signs (@%%@):
--
-- > > "memory usage: %i%%" -% 13
-- > "memory usage: 13%"
--
--
-- Percent signs are duplicated when using the '%' operator to allow chaining:
--
-- > > "percent sign: %s, memory usage: %i%%" % "%" % 87
-- > "percent sign: %%, memory usage: 87%%"
--
-- /Always/ use the '-%' operator when formatting the /last value/
-- to remove duplicate @%@ signs:
--
-- > > "percent sign: %s, memory usage: %i%%" % "%" -% 87
-- > "percent sign: %, memory usage: 87%"
--
-- To print, just prefix you format expression with @putStrLn $@:
--
-- > > putStrLn $ "Hello %s!" -% "World"
-- > Hello World!
module Text.PercentFormat
  ( (%)
  , (-%)
  , (/%)

  , (%%)
  , (%%%)
  , (%%%%)
  , (%%%%%)
  , (%%%%%%)

  , (-%%)
  , (-%%%)
  , (-%%%%)
  , (-%%%%%)
  , (-%%%%%%)

  , (+%)
  )
where

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (isDigit, toUpper)
import Text.PercentFormat.Spec  as S
import Text.PercentFormat.Utils hiding (align)
import Text.PercentFormat.Quotient (maybeReadQ, digits, Quotient, infinity, nan)
import qualified Text.PercentFormat.Quotient as Q
import qualified Text.PercentFormat.Utils as U
import Prelude hiding (showString, showChar)

(%) :: Show a => String -> a -> String
('%':s) % x =
  case ty sp of
  Percent     -> '%':'%': s' % x
  ReprSpec    -> (duplicatePercents $ showRepr    sp x) ++ s'
  StringSpec  -> (duplicatePercents $ showString  sp x) ++ s'
  CharSpec    -> (duplicatePercents $ showChar    sp x) ++ s'
  NumberSpec  -> (duplicatePercents $ showDigits  sp x) ++ s'
  where
  (sp,s') = parseSpec s
(c:s)       % x = c : s % x
""          % x = ""
infixl 9 %

showRepr :: Show a => Spec -> a -> String
showRepr spec = align spec . show

showString :: Show a => Spec -> a -> String
showString spec s =
  case maybeRead (show s) of
  Nothing -> align spec (show s)
  Just s  -> align spec s

showChar :: Show a => Spec -> a -> String
showChar spec c =
  case maybeRead (show c) of
  Nothing -> err '!' spec
  Just c  -> align spec (c:"")

-- TODO: refactor showDigits (currently very hacky)
showDigits :: Show a => Spec -> a -> String
showDigits spec x =
  case maybeReadQ (show x) of
  Nothing -> err '!' spec
  Just q -> applyWhen (padWith spec /= ' ') (signal q ++)
          . align' q
          . applyWhen (padWith spec == ' ') (signal q ++)
          . either id (\(ids,fds,pds) -> capitalize $ showds ids fds pds (precision spec))
          . digits (base spec)
          . round' (base spec) (precision spec)
          $ q
  where
  capitalize = applyWhen (capitalizeDigits spec) (map toUpper)
  signal q | q >= 0 = positivePrefix spec
           | q <  0 = "-"
  align' :: Quotient -> String -> String
  align' q = if padWith spec == ' '
               then align spec
               else align spec{width = width spec - length (signal q)}
  round' :: Int -> Maybe Int -> Quotient -> Quotient
  round' _ _ q | Q.isInfinite q = q
  round' _ _ q | Q.isNaN q      = q
  round' _ Nothing  q = q
  round' b (Just p) q = round (q * fromIntegral b ^ p) Q.% fromIntegral b ^ p
  showds :: [Int] -> [Int] -> [Int] -> Maybe Int -> String
  showds ids fds []  Nothing  | length fds < minPrecision spec
                            = showds ids (fds ++ replicate (minPrecision spec - length fds) 0) [] Nothing
  showds ids []  _   Nothing  = intsToDigits ids
  showds ids fds pds (Just 0) = intsToDigits ids
  showds ids fds pds Nothing  = intsToDigits ids ++ "."
                             ++ intsToDigits fds ++ showPeriod pds
  showds ids fds pds (Just pr) = intsToDigits ids ++ "."
                              ++ intsToDigits (take pr (fds ++ loop pds ++ repeat 0))
  showPeriod [] = ""
  showPeriod xs = intsToDigits xs
               ++ intsToDigits xs
               ++ intsToDigits xs
               ++ "..."

err :: Char -> Spec -> String
err c spec = align spec{padWith=c} (c:"")

(-%) :: Show a => String -> a -> String
s -% x = s % x /% '?'
infixl 9 -%

-- | Replaces "%%" by "%".  Any remaining occurrences of format strings are
--   replaced by the given error character.  Field width is respected when
--   possible.
--
-- > > "100%% %i" /% '?'
-- > "100% ?"
--
-- > > "100%% %03i" /% '?'
-- > "100% ???"
(/%) :: String -> Char -> String
s /% errChar = depercent s
  where
  depercent ('%':s) = let (spec,s') = parseSpec s
                          s'' = case ty spec of
                                Percent     -> "%"
                                _           -> err errChar spec
                      in s'' ++ depercent s'
  depercent (c:s) = c : depercent s
  depercent "" = ""

-- | Aligns a string following a given spec.
--
-- > align spec{width=1} "asdf"
-- "asdf"
-- > align spec{width=5} "asdf"
-- " asdf"
-- > align spec{width=5, leftAlign=True} "asdf"
-- "asdf "
align :: Spec -> String -> String
align spec = U.align (S.leftAlign spec) (padWith spec) (width spec)

duplicatePercents :: String -> String
duplicatePercents ('%':s) = '%':'%':duplicatePercents s
duplicatePercents (c:s)   = c:duplicatePercents s
duplicatePercents ""      = ""

(%%) :: (Show a, Show b) => String -> (a,b) -> String
s %% (x,y) = s % x % y

(%%%) :: (Show a, Show b, Show c) => String -> (a,b,c) -> String
s %%% (x,y,z) = s % x % y % z

(%%%%) :: (Show a, Show b, Show c, Show d) => String -> (a,b,c,d) -> String
s %%%% (x,y,z,w) = s % x % y % z % w

(%%%%%) :: (Show a, Show b, Show c, Show d, Show e)
        => String -> (a,b,c,d,e) -> String
s %%%%% (x,y,z,w,v) = s % x % y % z % w % v

(%%%%%%) :: (Show a, Show b, Show c, Show d, Show e, Show f)
         => String -> (a,b,c,d,e,f) -> String
s %%%%%% (x,y,z,w,v,u) = s % x % y % z % w % v % u

(-%%) :: (Show a, Show b) => String -> (a,b) -> String
s -%% t = s %% t /% '?'

(-%%%) :: (Show a, Show b, Show c) => String -> (a,b,c) -> String
s -%%% t = s %%% t /% '?'

(-%%%%) :: (Show a, Show b, Show c, Show d) => String -> (a,b,c,d) -> String
s -%%%% t = s %%%% t /% '?'

(-%%%%%) :: (Show a, Show b, Show c, Show d, Show e)
        => String -> (a,b,c,d,e) -> String
s -%%%%% t = s %%%%% t /% '?'

(-%%%%%%) :: (Show a, Show b, Show c, Show d, Show e, Show f)
         => String -> (a,b,c,d,e,f) -> String
s -%%%%%% t = s %%%%%% t /% '?'

-- | Just an alias to '%' for use whenever 'Data.Ratio' is in scope.
--
-- > import Data.Ratio
-- > import Text.PercentFormat hiding ((%))
-- > "..." +% 1 -% 2
(+%) :: Show a => String -> a -> String
(+%) = (%)
