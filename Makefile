# Makefile for percentformat
#
# Copyright:   (c) 2015-2018 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = \
  tests/test-spec \
  tests/test-unit \
  tests/test-number \
  tests/test-quotient \
  tests/test-prop
EGS =
BENCHS = bench/speculate
LISTHS   = find src -name \*.hs
LISTOBJS = $(LISTHS) | sed -e 's/.hs$$/.o/'
ALLHS    = $(shell $(LISTHS))
ALLOBJS  = $(shell $(LISTOBJS))
OBJS = src/Text/PercentFormat.o
GHCIMPORTDIRS = src:tests
GHCFLAGS = -O2 -dynamic
HADDOCKFLAGS = --no-print-missing-docs
HUGSIMPORTDIRS = .:./src:./tests:/usr/lib/hugs/packages/*

all: mk/toplibs $(TESTS)

test: $(patsubst %,%.test,$(TESTS)) diff-test

test-via-cabal:
	cabal clean  &&  cabal configure --enable-tests  && cabal test

test-via-stack:
	stack test

test-sdist:
	./tests/test-sdist

hlint:
	hlint \
	  .

doctest:
	doctest -isrc src/Text/PercentFormat.hs

bench: $(BENCHS)
	./bench/speculate

%.test: %
	./$<

clean: clean-hi-o
	rm -f $(TESTS) $(EG) mk/toplibs
	rm -rf doc/

ghci: mk/All.ghci

hugs: mk/All.hugs

diff-test: examples-diff-test

update-diff-test: update-examples-diff-test

examples-diff-test: bench/examples
	./bench/examples         | diff -rud tests/diff/examples.out     -
	./bench/examples "%r"    | diff -rud tests/diff/examples-r.out   -
	./bench/examples "%6r"   | diff -rud tests/diff/examples-6r.out  -
	./bench/examples "%-6r"  | diff -rud tests/diff/examples--6r.out -
	./bench/examples "%s"    | diff -rud tests/diff/examples-s.out   -
	./bench/examples "%6s"   | diff -rud tests/diff/examples-6s.out  -
	./bench/examples "%-6s"  | diff -rud tests/diff/examples--6s.out -
	./bench/examples "%i"    | diff -rud tests/diff/examples-i.out   -
	./bench/examples "%3i"   | diff -rud tests/diff/examples-3i.out  -
	./bench/examples "%-3i"  | diff -rud tests/diff/examples--3i.out -
	./bench/examples "%f"    | diff -rud tests/diff/examples-f.out   -
	./bench/examples "%c"    | diff -rud tests/diff/examples-c.out   -
	./bench/examples "abc"   | diff -rud tests/diff/examples-abc.out -

update-examples-diff-test: bench/examples
	./bench/examples         > tests/diff/examples.out
	./bench/examples "%r"    > tests/diff/examples-r.out
	./bench/examples "%6r"   > tests/diff/examples-6r.out
	./bench/examples "%-6r"  > tests/diff/examples--6r.out
	./bench/examples "%s"    > tests/diff/examples-s.out
	./bench/examples "%6s"   > tests/diff/examples-6s.out
	./bench/examples "%-6s"  > tests/diff/examples--6s.out
	./bench/examples "%i"    > tests/diff/examples-i.out   || true # TODO: fixme
	./bench/examples "%3i"   > tests/diff/examples-3i.out  || true # TODO: fixme
	./bench/examples "%-3i"  > tests/diff/examples--3i.out || true # TODO: fixme
	./bench/examples "%f"    > tests/diff/examples-f.out
	./bench/examples "%c"    > tests/diff/examples-c.out
	./bench/examples "abc"   > tests/diff/examples-abc.out

haddock: doc/index.html

clean-haddock:
	rm -f doc/*.{html,css,js,png,gif} README.html

upload-haddock:
	@echo "use \`cabal upload -d' instead"
	@echo "(but 1st: cabal install --only-dependencies --enable-documentation)"
	@echo "(to just compile docs: cabal haddock --for-hackage)"
	@echo "(on Arch Linux, use: cabal haddock --for-hackage --haddock-options=--optghc=-dynamic)"

doc/index.html: $(ALLHS)
	./mk/haddock-i base template-haskell | xargs \
	haddock --html -odoc $(ALLHS) $(HADDOCKFLAGS) --title=percent-format

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and tests/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

list-hs:
	$(LISTHS)

list-objs:
	$(LISTOBJS)

include mk/haskell.mk
