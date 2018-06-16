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
LISTHS   = find src mk tests bench -name \*.hs
LISTOBJS = $(LISTHS) | sed -e 's/.hs$$/.o/'
ALLHS    = $(shell $(LISTHS))
ALLOBJS  = $(shell $(LISTOBJS))
OBJS = src/Text/PercentFormat.o
GHCIMPORTDIRS = src:tests
GHCFLAGS = -O2 -dynamic
HADDOCKFLAGS = --no-print-missing-docs
HUGSIMPORTDIRS = .:./src:./tests:/usr/lib/hugs/packages/*

all: mk/toplibs $(TESTS)

test: $(patsubst %,%.test,$(TESTS))

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
