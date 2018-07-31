TO DO list for PercentFormat
============================

TO DO list for PercentFormat, more or less in order:

* on the `Makefile`:
	- add `hugs-test` target;

* when `%s` argument is char, generate `<char>` instead of `'<char>'`;

* when `%i`/`%f`/etc argument is a number encoded in a string, parse the number
  and make it work;

* add `%e`;

* release percent-format package on Hackage;

* add link to Haddock documentation on README;

* add `%q` (see `tests/test-number.hs`);

* sweep `TODO`s scattered through files;

* *maybe*: make "%l" for lists:

    > "%l" % [1,2,3] -- regular
    [1,2,3]
    > "% l" % [1,2,3] -- add spaces
    [1, 2, 3]
    > "%\nl" % [1,2,3] -- add linebreaks
    [ 1
    , 2
    , 3 ]

