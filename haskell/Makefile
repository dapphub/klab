all: build

default.nix: k-gas-analyser.cabal
	nix-shell --command 'cabal2nix . > default.nix'
.configured: default.nix shell.nix
	nix-shell --command 'cabal new-configure'
	touch .configured
build: .configured
	nix-shell --command 'cabal new-build --write-ghc-environment-files never'
repl: .configured
	nix-shell --command 'cabal new-repl --write-ghc-environment-files never'
tests: .configured
	nix-shell --command 'cabal new-test --write-ghc-environment-files never'

PORT ?= 8001
BROWSER ?= chromium
hoogle-server:; nix-shell --run 'hoogle server --local -p $(PORT)'

.PHONY: all build repl
