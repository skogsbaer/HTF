# THIS MAKEFILE SHOULD NOT BE USED TO BUILD AND INSTALL THE HTF!!
#
# See the README for build information
#
 
.PHONY: all clean configure build install

CABAL = runghc setup.hs

all: install

build:
	$(CABAL) build

configure:
	$(CABAL) configure --prefix=$(CURDIR)/usr

install: build
	$(CABAL) copy -v2
	$(CABAL) register --user -v2

clean:
	$(CABAL) clean
