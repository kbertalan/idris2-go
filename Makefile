ipkg=idris2-go.ipkg

.PHONY: build install run clean dev dev-build test-clean test-build test dev-test
 
build:
	pack build ${ipkg}

install: build
	idris2 --install ${ipkg}

run:
	pack run ${ipkg}

clean:
	rm -rf build/

dev:
	find src/ -name '*.idr' | entr -d bash -c 'time make run'

dev-build:
	find src/ -name '*.idr' | entr -d bash -c 'time make build'

test-clean:
	make -C tests clean

test-build:
	make -C tests build

test: install test-build
	make -C tests test

dev-test:
	find . -name *.idr | threads=4 INTERACTIVE="" entr make test

