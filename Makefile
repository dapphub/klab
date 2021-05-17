PATH:=$(PATH):$(CURDIR)/bin
KLAB_EVMS_PATH:=$(CURDIR)/evm-semantics
export PATH
export KLAB_EVMS_PATH

LIBEXEC_DIR=$(CURDIR)/libexec
HASKELL_DIR=$(CURDIR)/haskell
GAS_ANALYSER=$(CURDIR)/result/bin/k-gas-analyser

# shell output colouring:
red:=$(shell tput setaf 1)
green:=$(shell tput setaf 2)
yellow:=$(shell tput setaf 3)
bold:=$(shell tput bold)
reset:=$(shell tput sgr0)

default: deps

clean:
	rm -fdR out/* evm-semantics
	git submodule sync --recursive
	git submodule update --init --recursive evm-semantics

deps: deps-kevm deps-npm

deps-kevm:
	git submodule update --init --recursive evm-semantics
	cd evm-semantics \
		&& make k-deps tangle-deps -B \
		&& make build-java -B

deps-npm:
	npm install

deps-haskell:
	rm -f haskell/.ghc.environment*
	nix-build klab.nix
	ln -sf $(GAS_ANALYSER) $(LIBEXEC_DIR)/klab-gas-analyser

media: media/introduction.pdf

ci-resources: resources/report.tmp.html overview_html

overview_html: resources/overview.css

resources/overview.css: resources/overview.scss
	 ./node_modules/node-sass/bin/node-sass --source-map resources -o resources resources/overview.scss

overview_js:
	echo "done"

resources/report.tmp.html: resources/report.css
	# node -e 'const fs=require("fs"); const t=fs.readFileSync("./resources/report.tmp.csstmp.html", "utf8"); const s=t.replace("{{style}}", fs.readFileSync("./resources/report.css")); fs.writeFileSync("resources/report.tmp.html", s);'

resources/report.css: resources/report.scss
	 ./node_modules/node-sass/bin/node-sass --source-map resources -o resources resources/report.scss

media/%.pdf: media/%.md
	pandoc --from markdown --to beamer --output $@ $<

KLAB = $(CURDIR)/bin/klab

# doing them in this order should save time
examples := $(addprefix examples/,heal multipleCalls token SafeAdd multipleInternals createContract where)

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
	@ cd $* && klab prove-all && echo "$(green)$(bold)CHECKED$(reset) example: $* (with full gas analysis))"

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
