IDRIS2 ?= idris2

.PHONY: build install install-lib clean test-clean test-build test
 
build:
	time pack --no-prompt build idris2-go.ipkg

install:
	pack --no-prompt install-app idris2-go

install-lib:
	pack install idris2-go-lib

clean:
	rm -rf build

test-clean:
	make -C tests clean

test-build:
	make -C tests build

test:
	threads=4 make -C tests test

