# Idris2 Go

[Idris2 Go](https://github.com/kbertalan/idris2-go) is a library for generating [go](https://go.dev) programs and an [Idris2](https://idris-lang.org) compiler backend for generating __go__ programs from __Idris2__ source code.

This tool is for __experimental__ use only, relying on it in production systems is not advised.
While some Idris2 programs might work with this tool, there are major features missing, which can make the generated programs unstable or slow.

## Installation

The easiest way to install `idris2-go` is to use [idris2-pack](https://github.com/stefan-hoeck/idris2-pack):

```sh
pack install-app idris2-go
```

## Build

The provided Makefile also assumes the presense of [idris2-pack](https://github.com/stefan-hoeck/idris2-pack) on your system.

Currently it is developed on __linux__, using (almost) latest __Idris2__ and latest __go__ (version 1.20.x). If something is not working for you, then please consider to update your versions.

For the impatient you can check the below commands to build test runner and all tests using idris2-go:

```sh
make clean
make build
make install
make install-lib
make test-clean
IDRIS2=idris2-go make test-build
IDRIS2=idris2-go make test
```

## Current state and ideas

### Go code generator

- Language features
  - [x] Packages, imports
  - [x] Variables and constants
  - [x] Functions
  - [x] Structs, arrays, slices and maps
  - [x] Flow control
  - [ ] Methods
  - [ ] Interfaces
  - [ ] Generics
  - [ ] Concurrency
- Generator
  - [x] AST
  - [x] Combinators
  - [x] Code printer
  - [x] Tour of Go tests
  - [ ] Comment handling (it is incomplete, and alreay opinionated)

### Compiler

- [x] Whole program compiler
- [x] Expression compiler for REPL
- [x] Compile test lib, and run tests using it
- [x] Tail call optimozation from Idris2 ES
- [x] Compiles itself (about 2x slower than compiling with chez backend, and it uses about 10GB ram)
- [x] Basic FFI support
- [x] Specialize constructor cases
- [x] Reduce function literal calls if possible
- [ ] Windows support
- [ ] Mac support
- [ ] More mature FFI parsing and type support
- [ ] Implement missing FFI methods for libraries and Idris2 compiler
- [ ] Persistent data structures for list
- [ ] Performance tests and tuning

### Project

- [x] Install via pack
- [x] Build / Test pipeline

