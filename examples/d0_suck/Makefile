# increase this if you need to do more batches
BATCH_LIMIT ?= 5

SHELL ?= /usr/bin/env bash

# shell output colouring:
red:=$(shell tput setaf 1)
green:=$(shell tput setaf 2)
yellow:=$(shell tput setaf 3)
bold:=$(shell tput bold)
reset:=$(shell tput sgr0)

KLAB_OUT ?= $(CURDIR)/out
TMPDIR ?= $(CURDIR)/tmp
export KLAB_OUT
export TMPDIR
OBLIGATIONS = $(KLAB_OUT)/obligations.batch
BATCH_TIMESTAMP = $(KLAB_OUT)/batch.timestamp
SPEC_DIR = $(KLAB_OUT)/specs
GAS_DIR = $(KLAB_OUT)/gas

ifndef KLAB_EVMS_PATH
$(error $(red)Error$(reset): KLAB_EVMS_PATH must be defined and point to evm-semantics!)
endif

GAS_ANALYSER := $(shell klab gas-analyser --help 2>/dev/null)
ifndef GAS_ANALYSER
$(error $(red)Error$(reset): to use klab gas-analyser, please install the klab haskell component with `make deps-haskell`)
endif

# read obligations file if it exists
ifneq ("$(wildcard $(OBLIGATIONS))","")
obligation_spec_names = $(shell cat ${OBLIGATIONS})
obligation_specs = $(addsuffix .k,$(addprefix $(SPEC_DIR)/,$(obligation_spec_names)))
num_obligations = $(shell cat $(OBLIGATIONS) | wc -l)
endif


all: $(OBLIGATIONS)
	mkdir -p $(GAS_DIR)
	@ count=1; \
	while [ -s  $(OBLIGATIONS) ]; do \
		if [ $$count -gt $(BATCH_LIMIT) ]; then \
			echo "$(red)Exceeded batch limit, terminating!"; \
			exit 1 ; \
		fi ; \
		echo "$(bold)STARTING$(reset) proof batch $$count." ; \
		$(MAKE) -C ./ proof-batch || exit 1 ; \
		count=$$((count+1)); \
	done; \
        echo "$(bold)COMPLETE$(reset): no outstanding proof obligations."

$(BATCH_TIMESTAMP):

$(OBLIGATIONS): $(BATCH_TIMESTAMP)
	klab build

clean:
	rm -rf $(KLAB_OUT)/*

.SECONDEXPANSION:

proof-batch: $$(addsuffix .proof.timestamp,$$(obligation_specs))
	$(info $(bold)FINISHED$(reset) batch.)
	touch $(BATCH_TIMESTAMP)
	klab build

%_pass_rough.k.proof.timestamp: %_pass_rough.k $(OBLIGATIONS)
	time klab prove --dump $<
	klab status $<
	klab get-gas $$(klab hash $<) > $(GAS_DIR)/$(*F)_pass_rough.raw.kast.json
	klab gas-analyser --input $(GAS_DIR)/$(*F)_pass_rough.raw.kast.json > $(GAS_DIR)/$(*F)_pass_rough.kast

%.k.proof.timestamp: %.k $(OBLIGATIONS)
	time klab prove $<
