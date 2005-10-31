.PHONY: all clean

CABAL = runghc setup.hs

all:
	$(CABAL) build

install: 
	$(CABAL) install -v2
	$(CABAL) register -v2

clean:
	$(CABAL) clean