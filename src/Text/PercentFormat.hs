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
-- typeclasses -- it works on anything that is a 'Show' instance that produces
-- output in the supported formats.
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
--
--
-- == Supported formats
--
-- * /r/ -- 'show' representation as-is (including quotes for strings).
--
--     > > "%r" % "string"
--     > "\"string\""
--
--     > > "%r" % Just 10
--     > "Just 10"
--
-- * /s/ -- string.  If the argument is 'show'ed as a 'String', intersperse it,
--          otherwise include representation in whole.
--
--     > > ":%s:" % "string"
--     > ":string:"
--
--     > > ":%s:" % 10
--     > ":10:"
--
--     > > ":%s:" % Just "string"
--     > ":Just "string":"
--
-- * /c/ -- character.
--
--     > > "%c" % 'a'
--     > "a"
--
-- * /i/ -- Argument is converted to the nearest decimal integer.
--   Accepts arguments that when 'show'ed are represented as either
--   'Integer's, 'Rational's or 'Double's.
--
--     > > "%i" % 5040
--     > 5040
--     > > "%i" % 3.141
--     > 3
--
-- * /d/ -- Argument is converted to a decimal integer.
--   Accepts arguments that when 'show'ed are represented as either
--   'Integer's, 'Rational's or 'Double's.
--
--     > > "%d" % 5040
--     > 5040
--     > > "%i" % 3.141
--     > 3.141
--
-- * ...
--
--
-- == Supported flag charaters
--
-- TODO: TBA.
--
--
-- == How does it work?
--
-- TODO: TBA.
--
-- > data Digit = Zero | One | Two | Three
-- > instance Show Digit where
-- >   show Zero   =  "0"
-- >   show One    =  "1"
-- >   show Two    =  "2"
-- >   show Three  =  "3"
--
--
-- == Error Handling
--
-- This library is designed to avoid raising errors.
-- If conversion cannot be performed an exclamation mark (@!@) is produced.
-- If there are missing format strings an interrogation mark (@?@) is produced.
-- For example:
--
-- > > "%d %d" -% "Ten"
-- > "! ?"
--
-- The only two instances where errors are raised are:
--
-- 1. the argument values contain errors themselves:
--
--     > > "Hello %s!" % error "err"
--     > *** Exception err
--
--     > > error "err" % "World"
--     > *** Exception err
--
-- 2. the format string is not supported:
--
--     > > "%j" % 10
--     > *** Exception: unknown format string `j'
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

-- | Formats a single value into a string without finalizing:
--   leaving duplicate percent signs & remaining format sequences.
--
-- > > "Hello %s!" % "World"
-- > "Hello World!"
--
-- > > "processor usage: %d%%" % 67
-- > "processor usage: 67%%"
--
-- > > "load avg: %.2f %.2f %.2f" % 0.666
-- > "load avg: %0.67 %.2f %.2f"
--
-- Please use '-%' when formatting the last value into a string so that
-- duplicate percent signs are removed.
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

-- | Formats the last value into a string.
--   This finalizes formatting, removing duplicate percent signs and replacing
--   remaining format sequences with interrogation marks.
--
-- > > "Hello %s!" -% "World"
-- > "Hello World!"
--
-- > > "processor usage: %d%%" -% 67
-- > "processor usage: 67%"
--
-- > > "load avg: %.2f %.2f %.2f" % 0.666
-- > "load avg: %0.67 ? ?"
--
-- Please use '%' if you intend to further format values (chaining).
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

-- | Formats two values into a string without finalizing:
--   leaving duplicate percent signs & remaining format sequences.
--
-- > > "%s %s!" %% ("Hello","World")
-- > "Hello World!"
--
-- > > "load avg: %.2f %.2f %.2f" %% (0.666,0.333)
-- > "load avg: %0.67 %0.33 %.2f"
--
-- In general:
--
-- > s %% (x,y) == s % x % y
--
-- Please use '-%%' if you don't intend to format values into a string any further.
(%%) :: (Show a, Show b) => String -> (a,b) -> String
s %% (x,y) = s % x % y

-- | Formats three values into a string without finalizing.
--
-- > > "load avg: %.2f %.2f %.2f" %%% (0.666,0.333,0.1)
-- > "load avg: %0.67 %0.33 %0.10"
(%%%) :: (Show a, Show b, Show c) => String -> (a,b,c) -> String
s %%% (x,y,z) = s % x % y % z

-- | Formats four values into a string without finalizing.
(%%%%) :: (Show a, Show b, Show c, Show d) => String -> (a,b,c,d) -> String
s %%%% (x,y,z,w) = s % x % y % z % w

-- | Formats five values into a string without finalizing.
(%%%%%) :: (Show a, Show b, Show c, Show d, Show e)
        => String -> (a,b,c,d,e) -> String
s %%%%% (x,y,z,w,v) = s % x % y % z % w % v

-- | Formats six values into a string without finalizing.
(%%%%%%) :: (Show a, Show b, Show c, Show d, Show e, Show f)
         => String -> (a,b,c,d,e,f) -> String
s %%%%%% (x,y,z,w,v,u) = s % x % y % z % w % v % u

-- | Formats two values into a string and finalizes it:
--   removing duplicate percent signs & replacing remaining format sequences
--   with interrogation marks.
--
-- > > "%s %s!" -%% ("Hello","World")
-- > "Hello World!"
--
-- > > "load avg: %.2f %.2f %.2f" -%% (0.666,0.333)
-- > "load avg: %0.67 %0.33 ?"
--
-- In general:
--
-- > s -%% (x,y) == s % x -% y
--
-- Please use '%%' if you intend to further format values.
(-%%) :: (Show a, Show b) => String -> (a,b) -> String
s -%% t = s %% t /% '?'

-- | Formats three values into a string and finalizes it.
--
-- > > "load avg: %.2f %.2f %.2f" -%%% (0.666,0.333,0.1)
-- > "load avg: %0.67 %0.33 %0.10"
(-%%%) :: (Show a, Show b, Show c) => String -> (a,b,c) -> String
s -%%% t = s %%% t /% '?'

-- | Formats four values into a string and finalizes it.
(-%%%%) :: (Show a, Show b, Show c, Show d) => String -> (a,b,c,d) -> String
s -%%%% t = s %%%% t /% '?'

-- | Formats five values into a string and finalizes it.
(-%%%%%) :: (Show a, Show b, Show c, Show d, Show e)
        => String -> (a,b,c,d,e) -> String
s -%%%%% t = s %%%%% t /% '?'

-- | Formats six values into a stirng and finalizes it.
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
