ipkg=idris2-go.ipkg

.PHONY: build run clean dev dev-build

build:
	pack build ${ipkg}

run:
	pack run ${ipkg}

clean:
	rm -rf build/

dev:
	find -name '*.idr' | entr -d bash -c 'time make run'

dev-build:
	find -name '*.idr' | entr -d bash -c 'time make build'

