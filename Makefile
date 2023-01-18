ipkg=idris2-go.ipkg

.PHONY: build install run clean dev dev-build test-clean test-build test dev-test compile-test compile-self
 
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

test: test-build
	make -C tests test

dev-test:
	find . -name *.idr | threads=4 INTERACTIVE="" entr make test

build/exec/idris2-go:
	make clean
	make build

tests/build/exec/runtests: build/exec/idris2-go
	make test-clean
	IDRIS2_DATA="${PWD}/support" ./build/exec/idris2-go --build tests/runtests.ipkg --directive module=github.com/kbertalan/idris2-go/tests
	cd tests/build/exec && go mod init github.com/kbertalan/idris2-go/tests && go mod tidy && go build -o runtests

run-go-test: build/exec/idris2-go tests/build/exec/runtests
	cd tests && IDRIS2_DATA="${PWD}/support" ./build/exec/runtests "${PWD}/build/exec/idris2-go" --timing --failure-file tests/failures --threads 1 --only "go/"

compile-test: build/exec/idris2-go tests/build/exec/runtests
	cd tests && IDRIS2_DATA="${PWD}/support" ./build/exec/runtests "${PWD}/build/exec/idris2-go" --timing --failure-file tests/failures --threads 2 --only "tour/basics10"

compile-self:
	make clean
	bash -c "time make build"
	mkdir build/go
	mv ./build/exec/idris2-go* ./build/go/
	rm -rf ./build/exec ./build/ttc
	IDRIS2_DATA="${PWD}/support" bash -c "time ./build/go/idris2-go --build idris2-go.ipkg --directive module=github.com/kbertalan/idris2-go"
	(cd build/exec && go mod init github.com/kbertalan/idris2-go && go build -o idris2-go)

