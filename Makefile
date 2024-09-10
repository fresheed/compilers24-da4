# Makefile for running dune build

.PHONY: all build clean

all: build

# Build target
build:
	dune build

# Clean target, run dune clean
clean:
	dune clean
