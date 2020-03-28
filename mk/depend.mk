bench/examples: \
  bench/examples.hs \
  mk/toplibs
bench/examples.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs \
  bench/examples.hs
bench/speculate: \
  bench/speculate.hs \
  mk/toplibs
bench/speculate.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs \
  bench/speculate.hs
mk/All.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs \
  mk/Toplibs.hs
src/Text/PercentFormat: \
  mk/toplibs
src/Text/PercentFormat.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
src/Text/PercentFormat/Quotient.o: \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Quotient.hs
src/Text/PercentFormat/Spec.o: \
  src/Text/PercentFormat/Spec.hs
src/Text/PercentFormat/Utils.o: \
  src/Text/PercentFormat/Utils.hs
tests/test-number.o: \
  tests/Test.hs \
  tests/test-number.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
tests/test-number: \
  tests/Test.hs \
  tests/test-number.hs \
  mk/toplibs
tests/Test.o: \
  tests/Test.hs
tests/test-prop.o: \
  tests/test-prop.hs \
  tests/Test.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
tests/test-prop: \
  tests/test-prop.hs \
  tests/Test.hs \
  mk/toplibs
tests/test-quotient.o: \
  tests/test-quotient.hs \
  tests/Test.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Quotient.hs
tests/test-quotient: \
  tests/test-quotient.hs \
  tests/Test.hs \
  mk/toplibs
tests/test-scientific.o: \
  tests/test-scientific.hs \
  tests/Test.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
tests/test-scientific: \
  tests/test-scientific.hs \
  tests/Test.hs \
  mk/toplibs
tests/test-spec.o: \
  tests/test-spec.hs \
  tests/Test.hs \
  src/Text/PercentFormat/Spec.hs
tests/test-spec: \
  tests/test-spec.hs \
  tests/Test.hs \
  mk/toplibs
tests/test-unit.o: \
  tests/test-unit.hs \
  tests/Test.hs \
  src/Text/PercentFormat/Utils.hs \
  src/Text/PercentFormat/Spec.hs \
  src/Text/PercentFormat/Quotient.hs \
  src/Text/PercentFormat.hs
tests/test-unit: \
  tests/test-unit.hs \
  tests/Test.hs \
  mk/toplibs
