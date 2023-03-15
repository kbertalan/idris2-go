IDRIS2 ?= idris2
IDRIS2_PACKAGE_PATH ?= `pack package-path`
IDRIS2_LIBS ?= `pack libs-path`
package=idris2-go
ipkg=${package}.ipkg

.PHONY: build scheme-build go-build install-lib clean test-clean test-build scheme-test-build go-test-build test scheme-test go-test
 
build: scheme-build

# build idris2-go, which will be a scheme executable
scheme-build: build/scheme/idris2-go

# build idris2-go, which will be a native (go) executable
go-build: build/go/idris2-go

# install as library, needed for running tests
install-lib:
	${IDRIS2} --install ${ipkg}

clean:
	rm -rf build

test-clean:
	make -C tests clean

test-build:
	make -C tests build

# build test executable using idris2-go (scheme executable) to product native (go) executable
scheme-test-build:
	IDRIS2="${PWD}/build/scheme/idris2-go" IDRIS2_DATA="${PWD}/support" make -C tests build

# build test executable using idris2-go (go executable) to product native (go) executable
go-test-build:
	IDRIS2="${PWD}/build/go/idris2-go" IDRIS2_DATA="${PWD}/support" make -C tests build

# test using your installed idris2 executable
test:
	threads=4 make -C tests test

# test using idris2-go (scheme executable) which builds go executables for tests
scheme-test: build/scheme/idris2-go
	IDRIS2="${PWD}/build/scheme/idris2-go" \
		IDRIS2_DATA="${PWD}/support" \
		IDRIS2_PACKAGE_PATH=${IDRIS2_PACKAGE_PATH} \
		IDRIS2_LIBS=${IDRIS2_LIBS} \
		threads=4 make -C tests test

# test using idris2-go (go executable) which builds go executables for tests
go-test: build/go/idris2-go
	IDRIS2="${PWD}/build/go/idris2-go" \
		IDRIS2_DATA="${PWD}/support" \
		IDRIS2_PACKAGE_PATH=${IDRIS2_PACKAGE_PATH} \
		IDRIS2_LIBS=${IDRIS2_LIBS} \
		threads=4 make -C tests test


build/exec/idris2-go:
	echo yes | time pack build ${ipkg}

build/scheme/idris2-go: build/exec/idris2-go
	mkdir -p build/scheme
	cp -ar build/exec/idris2-go* build/scheme/

build/go/idris2-go: build/scheme/idris2-go
	rm -rf ./build/exec ./build/ttc
	IDRIS2_DATA="${PWD}/support" time ./build/scheme/idris2-go --build ${ipkg}
	mkdir -p build/go
	cp -ar ./build/exec/idris2-go ./build/go/

