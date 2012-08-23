.PHONY: run

run:
	cabal configure
	cabal build
	dist/build/test/test
	@echo
	@echo "EXPECTED:"
	@echo "* Tests:    41"
	@echo "* Passed:   12"
	@echo "* Pending:  2"
	@echo "* Failures: 26"
	@echo "* Errors:   1"
