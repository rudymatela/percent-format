TO DO list for PercentFormat
============================

TO DO list for PercentFormat, more or less in order:

* sweep `TODO`s scattered through files;

* test on travis;

* on the `Makefile`:
	- add `legacy-test` target;
	- add `legacy-test-via-cabal` target;
	- add `hugs-test` target;

* add `%e`;

* release percent-format package on Hackage;

* add link to Haddock documentation on README;

* add `%q` (see `tests/test-number.hs`);

* *maybe*: make "%l" for lists:

    > "%l" % [1,2,3] -- regular
    [1,2,3]
    > "% l" % [1,2,3] -- add spaces
    [1, 2, 3]
    > "%\nl" % [1,2,3] -- add linebreaks
    [ 1
    , 2
    , 3 ]

