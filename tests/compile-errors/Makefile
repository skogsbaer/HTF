.PHONY: check

FLAGS = -hide-all-packages -package HTF -package base --make

check:
	ghc $(FLAGS) Test1.hs || ghc $(FLAGS) Test2.hs || true
	@echo
	@echo "==> Both files should have compile errors at line 7"
