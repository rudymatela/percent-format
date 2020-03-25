# Makefile for percentformat
#
# Copyright:   (c) 2015-2018 Rudy Matela
# License:     3-Clause BSD  (see the file LICENSE)
# Maintainer:  Rudy Matela <rudy@matela.com.br>
TESTS = \
  tests/test-spec \
  tests/test-unit \
  tests/test-number \
  tests/test-scientific \
  tests/test-quotient \
  tests/test-prop
EGS =
BENCHS = bench/speculate
GHCIMPORTDIRS = src:tests
GHCFLAGS = -O2 $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
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

legacy-test: # needs ghc-8.0 .. ghc-7.8 installed as such
	make clean  &&  make test GHC=ghc-8.2  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-8.0  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-7.10 GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test GHC=ghc-7.8  GHCFLAGS="-Werror -dynamic"
	make clean  &&  make test

legacy-test-via-cabal: # needs similarly named cabal wrappers
	cabal clean  &&  cabal-ghc-8.2  configure  &&  cabal-ghc-8.2  test
	cabal clean  &&  cabal-ghc-8.0  configure  &&  cabal-ghc-8.0  test
	cabal clean  &&  cabal-ghc-7.10 configure  &&  cabal-ghc-7.10 test
	cabal clean  &&  cabal-ghc-7.8  configure  &&  cabal-ghc-7.8  test
	cabal clean  &&  cabal test

prepare:
	cabal update  &&  cabal install leancheck

prepare-legacy-test:
	cabal-ghc-8.2  update  &&  cabal-ghc-8.2  install leancheck
	cabal-ghc-8.0  update  &&  cabal-ghc-8.0  install leancheck
	cabal-ghc-7.10 update  &&  cabal-ghc-7.10 install leancheck
	cabal-ghc-7.8  update  &&  cabal-ghc-7.8  install leancheck

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

hugs-test: \
  tests/test-spec.runhugs \
  tests/test-unit.runhugs \
  tests/test-number.runhugs \
  tests/test-scientific.runhugs

diff-test: examples-diff-test

update-diff-test: update-examples-diff-test

examples-diff-test: bench/examples
	./bench/examples             | diff -rud tests/diff/examples.out         -
	./bench/examples "%r"        | diff -rud tests/diff/examples-r.out       -
	./bench/examples "%6r"       | diff -rud tests/diff/examples-6r.out      -
	./bench/examples "%-6r"      | diff -rud tests/diff/examples--6r.out     -
	./bench/examples "%s"        | diff -rud tests/diff/examples-s.out       -
	./bench/examples "%6s"       | diff -rud tests/diff/examples-6s.out      -
	./bench/examples "%-6s"      | diff -rud tests/diff/examples--6s.out     -
	./bench/examples "%i"        | diff -rud tests/diff/examples-i.out       -
	./bench/examples "%3i"       | diff -rud tests/diff/examples-3i.out      -
	./bench/examples "%03i"      | diff -rud tests/diff/examples-03i.out     -
	./bench/examples "%-3i"      | diff -rud tests/diff/examples--3i.out     -
	./bench/examples "%+3i"      | diff -rud tests/diff/examples-+3i.out     -
	./bench/examples "% 3i"      | diff -rud tests/diff/examples-_3i.out     -
	./bench/examples "%f"        | diff -rud tests/diff/examples-f.out       -
	./bench/examples "%5f"       | diff -rud tests/diff/examples-5f.out      -
	./bench/examples "%+5f"      | diff -rud tests/diff/examples-+5f.out     -
	./bench/examples "%-5f"      | diff -rud tests/diff/examples--5f.out     -
	./bench/examples "%11.2f"    | diff -rud tests/diff/examples-11p2f.out   -
	./bench/examples "%-11.2f"   | diff -rud tests/diff/examples--11p2f.out  -
	./bench/examples "%c"        | diff -rud tests/diff/examples-c.out       -
	./bench/examples "%2c"       | diff -rud tests/diff/examples-2c.out      -
	#./bench/examples "%b"        | diff -rud tests/diff/examples-b.out       - # TODO: FIXME: infinite loop
	#./bench/examples "%o"        | diff -rud tests/diff/examples-o.out       - # TODO: FIXME: infinite loop
	#./bench/examples "%x"        | diff -rud tests/diff/examples-x.out       - # TODO: FIXME: infinite loop
	#./bench/examples "%X"        | diff -rud tests/diff/examples-X_.out      - # TODO: FIXME: infinite loop
	./bench/examples "abc"       | diff -rud tests/diff/examples-abc.out     -

update-examples-diff-test: bench/examples
	./bench/examples             > tests/diff/examples.out
	./bench/examples "%r"        > tests/diff/examples-r.out
	./bench/examples "%6r"       > tests/diff/examples-6r.out
	./bench/examples "%-6r"      > tests/diff/examples--6r.out
	./bench/examples "%s"        > tests/diff/examples-s.out
	./bench/examples "%6s"       > tests/diff/examples-6s.out
	./bench/examples "%-6s"      > tests/diff/examples--6s.out
	./bench/examples "%i"        > tests/diff/examples-i.out
	./bench/examples "%3i"       > tests/diff/examples-3i.out
	./bench/examples "%03i"      > tests/diff/examples-03i.out
	./bench/examples "%-3i"      > tests/diff/examples--3i.out
	./bench/examples "%+3i"      > tests/diff/examples-+3i.out
	./bench/examples "% 3i"      > tests/diff/examples-_3i.out
	./bench/examples "%f"        > tests/diff/examples-f.out
	./bench/examples "%5f"       > tests/diff/examples-5f.out
	./bench/examples "%+5f"      > tests/diff/examples-+5f.out
	./bench/examples "%-5f"      > tests/diff/examples--5f.out
	./bench/examples "%11.2f"    > tests/diff/examples-11p2f.out
	./bench/examples "%-11.2f"   > tests/diff/examples--11p2f.out
	./bench/examples "%c"        > tests/diff/examples-c.out
	./bench/examples "%2c"       > tests/diff/examples-2c.out
	#./bench/examples "%b"        > tests/diff/examples-b.out  # TODO: FIXME: infinite loop
	#./bench/examples "%o"        > tests/diff/examples-o.out  # TODO: FIXME: infinite loop
	#./bench/examples "%x"        > tests/diff/examples-x.out  # TODO: FIXME: infinite loop
	#./bench/examples "%X"        > tests/diff/examples-X_.out # TODO: FIXME: infinite loop
	./bench/examples "abc"       > tests/diff/examples-abc.out

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and tests programs so long as they don't share dependencies _not_ stored
# in src/ and tests/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk
