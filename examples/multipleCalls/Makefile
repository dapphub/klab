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

%.k.proof.timestamp: %.k $(OBLIGATIONS)
	klab prove --dump $<
	klab get-gas $< > $(GAS_DIR)/$(*F).raw.kast.json
	klab gas-analyser --input $(GAS_DIR)/$(*F).raw.kast.json > $(GAS_DIR)/$(*F).kast

