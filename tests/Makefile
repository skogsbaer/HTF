.PHONY: run

run:
	cabal configure
	cabal build
	dist/build/test/test
	@echo
	@echo "EXPECTED:"
	@echo "* Tests:    43"
	@echo "* Passed:   13"
	@echo "* Pending:  2"
	@echo "* Failures: 28"
	@echo "* Errors:   1"
	@cd compile-errors && make check
