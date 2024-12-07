.PHONY: all build clean test install uninstall doc fmt

all: build

build:
	dune build @install

clean:
	dune clean

test:
	dune runtest

install:
	dune install

uninstall:
	dune uninstall

# Run the executable
run:
	dune exec orange

# Watch for changes and rebuild
watch:
	dune build -w

# Generate documentation
doc:
	dune build @doc

# Format source code
fmt:
	dune build @fmt --auto-promote

# Start utop with project libraries loaded
utop:
	dune utop

.DEFAULT_GOAL := all
