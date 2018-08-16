PATH:=$(PATH):$(CURDIR)/bin
KLAB_EVMS_PATH:=$(CURDIR)/evm-semantics
TMPDIR=$(CURDIR)/tmp
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

media: media/introduction.pdf

media/%.pdf: media/%.md
	pandoc --from markdown --to beamer --output $@ $<

test_dir=examples
fail_dir=examples/should_err
tests=$(wildcard $(test_dir)/*)
fail_tests=$(wildcard $(fail_dir)/*)

test:  $(tests:=.test)
	pkill klab

pre-test:
	klab server & mkdir -p $(TMPDIR) 

%.test: pre-test
	cd $* && klab run --headless --force

# Tests that should fail
%.fail_test:
	cd $* && klab run --headless --force && ([ $$? -eq 0 ] && echo "error! should have failed!)!") || echo "Exits with nonzero exit code as expected" 


SHELL = bash
dirs = {bin,libexec}
prefix ?= /usr/local

dirs:; mkdir -p $(prefix)/$(dirs)
files = $(shell ls -d $(dirs)/*)
link: uninstall dirs; npm i; for x in $(files); do \
	ln -s `pwd`/$$x $(prefix)/$$x; done
uninstall:; rm -rf $(addprefix $(prefix)/,$(files))
