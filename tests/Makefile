BINS = test-unit-tests

GHC = htf-ghc

.PHONY: all
all: $(BINS)

test-unit-tests: TestUnitTests.hs
	$(GHC) -o $@ --make $<

.PHONY: clean
clean:
	rm -f $(BINS) $(wildcard *.o *.hi)