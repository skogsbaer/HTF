BINS = tt

GHC = PATH=../usr/bin:$$PATH htf-ghc 

.PHONY: all
all: $(BINS)

tt: TestHTF.hs
	$(GHC) -o $@ --make $<

.PHONY: clean
clean:
	rm -f $(BINS) $(wildcard *.o *.hi)