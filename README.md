PercentFormat -- C-like printf-style string formatting for Haskell
==================================================================

[![PercentFormat's Build Status][build-status]][build-log]
[![PercentFormat on Hackage][hackage-version]][percent-format-on-hackage]

The `Text.PercentFormat` library provides printf-style string formatting.  It
provides a `%` operator (as in Ruby or Python) and uses the old C-printf-style
format you know and love.

This library differs from `Text.Printf` in that it does not rely on custom
typeclasses -- it works on anything that is a `Show` instance.

Formatting one value:

	> import Text.PercentFormat
	> "Hello %s!" -% "World"
	"Hello World!"

Formatting three values, tuple style:

	> "load average: %1.2f %1.2f %1.2f" -%%% (0.00, 0.066, 0.11)
	"load average: 0.00 0.07 0.11"

Formatting three values, chain style:

	> "load average: %1.2f %1.2f %1.2f" % 0.00 % 0.066 -% 0.11
	"load average: 0.00 0.07 0.11"


To produce a string with a percent sign (`%`),
use two percent signs (`%%`):

	> "memory usage: %i%%" -% 13
	"memory usage: 13%"


Percent signs are duplicated when using the `%` operator to allow chaining:

	> "percent sign: %s, memory usage: %i%%" % "%" % 87
	"percent sign: %%, memory usage: 87%%"

_Always_ use the `-%` operator when formatting the _last value_
to remove duplicate `%` signs:

	> "percent sign: %s, memory usage: %i%%" % "%" -% 87
	"percent sign: %, memory usage: 87%"

To print, just prefix you format expression with `putStrLn $`:

	> putStrLn $ "Hello %s!" -% "World"
	Hello World!

[build-status]: https://travis-ci.org/rudymatela/percent-format.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/percent-format
[hackage-version]: https://img.shields.io/hackage/v/percent-format.svg
[percent-format-on-hackage]: https://hackage.haskell.org/package/percent-format
