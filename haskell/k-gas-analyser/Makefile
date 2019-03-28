NIX_TARGET=result/bin/k-gas-analyser

all: build

build: $(NIX_TARGET)

$(NIX_TARGET): default.nix
	nix-build

test: test-help

test-help: $(NIX_TARGET)
	./$(NIX_TARGET) --help
