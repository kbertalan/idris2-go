ipkg=idris2-go.ipkg

.PHONY: build install run clean dev dev-build test-clean test-build test dev-test go-simple-test go-test compile-self
 
build:
	echo yes | pack build ${ipkg}

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

test:
	make -C tests test

dev-test:
	find . -name *.idr | threads=4 INTERACTIVE="" entr make test

build/exec/idris2-go:
	make clean
	time make build

tests/build/exec/runtests: build/exec/idris2-go
	make test-clean
	./build/exec/idris2-go --build tests/runtests.ipkg

go-simple-test: build/exec/idris2-go tests/build/exec/runtests
	cd tests && IDRIS2_DATA="${PWD}/support" ./build/exec/runtests "${PWD}/build/exec/idris2-go" --interactive --timing --failure-file failures --threads 4 --only "go/"

go-test: build/exec/idris2-go tests/build/exec/runtests
	cd tests && IDRIS2_DATA="${PWD}/support" ./build/exec/runtests "${PWD}/build/exec/idris2-go" --timing --failure-file failures --threads 4

build/go/idris2-go: build/exec/idris2-go
	mkdir build/go
	mv ./build/exec/idris2-go* ./build/go/

compile-self: build/go/idris2-go
	rm -rf ./build/exec ./build/ttc
	IDRIS2_DATA="${PWD}/support" time ./build/go/idris2-go --build idris2-go.ipkg

