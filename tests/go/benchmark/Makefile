IDRIS2 ?= idris2-go
only ?= .
time ?= 1s

.PHONY: clean
clean:
	rm -rf ./build

.PHONY: build
build:
	$(IDRIS2) --build test.ipkg

.PHONY: benchmark
benchmark: clean build
	cp *_test.go ./build/exec/test-src
	cd ./build/exec/test-src; go test -bench=$(only) -benchmem -cpuprofile ../../../cpu.out -memprofile ../../../mem.out -benchtime=$(time)

.PHONY: escapes
escapes: clean build
	cp *_test.go ./build/exec/test-src
	cd ./build/exec/test-src; go test -gcflags='-m -m' -bench=$(only) 2> ../../../escapes.out

.PHONY: memprof
memprof:
	go tool pprof -http : mem.out

.PHONY: cpuprof
cpuprof:
	go tool pprof -http : cpu.out

