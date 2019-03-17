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
	git submodule sync --recursive
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
	ln -s $(GAS_SOLVER) $(LIBEXEC_DIR)/klab-gas-analyser

media: media/introduction.pdf

media/%.pdf: media/%.md
	pandoc --from markdown --to beamer --output $@ $<

KLAB = $(CURDIR)/bin/klab

start-server: server.PID
	$(info $(bold)STARTED$(reset) KLab server.)

server.PID:
	mkdir -p $(TMPDIR) && { nohup $(KLAB) server > nohup.out 2>&1 & echo $$! > $@; }

stop-server: server.PID
	kill -- -$$(ps -o pgid= `cat $<` | grep -o '[0-9]*') && rm $< && echo "$(bold)STOPPED$(reset) Klab server."

examples=$(wildcard examples/*)

build-test: $(examples:=.build)

%.build:
	cd $* && $(KLAB) build

# workaround for patsubst in pattern matching target below
PERCENT := %

.SECONDEXPANSION:

test-with-gas: $(examples:=.example-with-gas)
	$(info $(bold)CHECKED$(reset) all example specs, with full gas analysis.)

%.example-with-gas:
	$(info Moving to example: $*)
	@ $(MAKE) -C $* && echo "$(green)$(bold)CHECKED$(reset) example: $* (with full gas analysis))"

test: test-without-gas

test-without-gas: $$(patsubst $$(PERCENT),$$(PERCENT).proof,$$(wildcard $(CURDIR)/examples/*/out/specs/*.k))
	$(info $(bold)CHECKED$(reset) all test specs (without gas analysis).)

%.k.proof: %.k
	$(info Proof $(bold)STARTING$(reset): $<)
	@ cd $(dir $*)../../ && $(KLAB) debug --headless --force $< && echo "$(green)Proof $(bold)ACCEPTED(reset): $<"

SHELL = bash
DIRS = {bin,libexec}
PREFIX ?= /usr/local

DIRS:; mkdir -p $(PREFIX)/$(DIRS)
files = $(shell ls -d $(DIRS)/*)
link: uninstall DIRS; for x in $(files); do \
	ln -s `pwd`/$$x $(PREFIX)/$$x; done
uninstall:; rm -rf $(addprefix $(PREFIX)/,$(files))
