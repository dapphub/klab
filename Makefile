PATH:=$(PATH):$(CURDIR)/bin
KLAB_EVMS_PATH:=$(CURDIR)/evm-semantics
export PATH
export KLAB_EVMS_PATH

LIBEXEC_DIR=$(CURDIR)/libexec
HASKELL_DIR=$(CURDIR)/haskell
GAS_SOLVER=$(HASKELL_DIR)/result/bin/k-gas-analyser

# shell output colouring:
red:=$(shell tput setaf 1)
green:=$(shell tput setaf 2)
yellow:=$(shell tput setaf 3)
bold:=$(shell tput bold)
reset:=$(shell tput sgr0)

default: deps

clean:
	rm -fdR out/* evm-semantics $(TMPDIR)/klab
	git submodule update --init -- evm-semantics

deps: deps-kevm deps-npm

deps-kevm:
	git submodule update --init -- evm-semantics
	cd evm-semantics \
		&& make k-deps tangle-deps -B \
		&& make build-java -B

deps-npm:
	npm install

deps-haskell:
	cd haskell/ && make
	ln -sf $(GAS_SOLVER) $(LIBEXEC_DIR)/klab-gas-analyser

media: media/introduction.pdf

media/%.pdf: media/%.md
	pandoc --from markdown --to beamer --output $@ $<

KLAB = $(CURDIR)/bin/klab

examples=$(wildcard examples/*)

# this is now redundant
build-test:

clean-test: $(examples:=.clean)

%.clean:
	$(MAKE) -C $* clean

# workaround for patsubst in pattern matching target below
PERCENT := %

.SECONDEXPANSION:

test-with-gas: $(examples:=.example-with-gas)
	$(info $(bold)CHECKED$(reset) all example specs, with full gas analysis.)

%.example-with-gas:
	$(info Moving to example: $*)
	@ $(MAKE) -C $* && echo "$(green)$(bold)CHECKED$(reset) example: $* (with full gas analysis))"

test: test-with-gas

test-without-gas: $$(patsubst $$(PERCENT),$$(PERCENT).proof,$$(wildcard $(CURDIR)/examples/*/out/specs/*.k))
	$(info $(bold)CHECKED$(reset) all test specs (without gas analysis).)

%.k.proof: %.k
	cd $(dir $*)../../ && $(KLAB) prove --dump $<

SHELL = bash
DIRS = {bin,libexec}
PREFIX ?= /usr/local

DIRS:; mkdir -p $(PREFIX)/$(DIRS)
files = $(shell ls -d $(DIRS)/*)
link: uninstall DIRS; for x in $(files); do \
	ln -s `pwd`/$$x $(PREFIX)/$$x; done
uninstall:; rm -rf $(addprefix $(PREFIX)/,$(files))
