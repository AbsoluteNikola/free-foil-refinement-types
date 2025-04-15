generate:
	cabal exec bash --  generate.sh

.PHONY: test
test:
	python3 test.py
