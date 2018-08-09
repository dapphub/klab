PATH:=$(PATH):$(CURDIR)/bin
KLAB_EVMS_PATH:=$(CURDIR)/evm-semantics
TMPDIR=/tmp
export PATH
export KLAB_EVMS_PATH
export TMPDIR

default: link

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

test_k:
	./test.sh

test_dir:=test
test_filles:=$(wildcard $(test_dir)/*.js)

test: $(test_files:=.test)

$(test_dir)/%.test:
	node_modules/mocha/bin/mocha $(test_dir)/$*

media: media/introduction.pdf

media/%.pdf: media/%.md
	pandoc --from markdown --to beamer --output $@ $<

test_dir=examples
tests=$(wildcard $(test_dir)/*)

test: $(tests:=.test)

%.test:
	cd $* && klab run --headless

SHELL = bash
dirs = {bin,libexec}
prefix ?= /usr/local

dirs:; mkdir -p $(prefix)/$(dirs)
files = $(shell ls -d $(dirs)/*)
link: uninstall dirs; npm i; for x in $(files); do \
	ln -s `pwd`/$$x $(prefix)/$$x; done
uninstall:; rm -rf $(addprefix $(prefix)/,$(files))
