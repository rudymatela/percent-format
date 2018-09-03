PercentFormat -- C-like printf-style string formatting for Haskell
==================================================================

[![PercentFormat's Build Status][build-status]][build-log]
[![PercentFormat on Hackage][hackage-version]][percent-format-on-hackage]
[![PercentFormat on Stackage LTS][stackage-lts-badge]][percent-format-on-stackage-lts]
[![PercentFormat on Stackage Nightly][stackage-nightly-badge]][percent-format-on-stackage-nightly]

The `Text.PercentFormat` library provides printf-style string formatting.  It
provides a [`%`] operator (as in Ruby or Python) and uses the old C-printf-style
format you know and love.

This library differs from `Text.Printf` in that it does not rely on custom
typeclasses -- it works on anything that is a `Show` instance.

Formatting one value:

	> import Text.PercentFormat
	> "Hello %s!" -% "World"
	"Hello World!"

Formatting three values, tuple style, using [`-%%%`]:

	> "load average: %1.2f %1.2f %1.2f" -%%% (0.00, 0.066, 0.11)
	"load average: 0.00 0.07 0.11"

Formatting three values, chain style, using [`%`] and [`-%`]:

	> "load average: %1.2f %1.2f %1.2f" % 0.00 % 0.066 -% 0.11
	"load average: 0.00 0.07 0.11"


To produce a string with a percent sign (`%`),
use two percent signs (`%%`):

	> "memory usage: %i%%" -% 13
	"memory usage: 13%"


Percent signs are duplicated when using the [`%`] operator to allow chaining:

	> "percent sign: %s, memory usage: %i%%" % "%" % 87
	"percent sign: %%, memory usage: 87%%"

_Always_ use the [`-%`] operator when formatting the _last value_
to remove duplicate `%` signs:

	> "percent sign: %s, memory usage: %i%%" % "%" -% 87
	"percent sign: %, memory usage: 87%"

To print, just prefix you format expression with `putStrLn $`:

	> putStrLn $ "Hello %s!" -% "World"
	Hello World!

For more information and a detailed list of options, see [PercentFormat's
Haddock Documentation].

PercentFormat is a work in progress.  Any help or pull requests are welcome.
See [PercentFormat's TO DO list] for ideas on how to contribute.


Installing
----------

To install the latest PercentFormat version from Hackage, just run:

	$ cabal update
	$ cabal install percent-format


[build-status]: https://travis-ci.org/rudymatela/percent-format.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/percent-format
[hackage-version]: https://img.shields.io/hackage/v/percent-format.svg
[percent-format-on-hackage]: https://hackage.haskell.org/package/percent-format
[PercentFormat's Haddock Documentation]: https://hackage.haskell.org/package/percent-format/docs/Text-PercentFormat.html
[`%`]: https://hackage.haskell.org/package/percent-format-0.0.1/docs/Text-PercentFormat.html#v:-37-
[`-%`]: https://hackage.haskell.org/package/percent-format-0.0.1/docs/Text-PercentFormat.html#v:-45--37-
[`-%%%`]: https://hackage.haskell.org/package/percent-format-0.0.1/docs/Text-PercentFormat.html#v:-45--37--37--37-
[PercentFormat's TO DO list]: TODO.md
[stackage-lts-badge]:                 http://stackage.org/package/percent-format/badge/lts
[stackage-nightly-badge]:             http://stackage.org/package/percent-format/badge/nightly
[percent-format-on-stackage]:         http://stackage.org/package/percent-format
[percent-format-on-stackage-lts]:     http://stackage.org/lts/package/percent-format
[percent-format-on-stackage-nightly]: http://stackage.org/nightly/package/percent-format
