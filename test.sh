#!/usr/bin/env bash
KLAB_EVMS_PATH=$(pwd)/evm-semantics
export PATH=$PATH:$(pwd)/bin
klab server  & cd examples/SafeAdd && klab run --headless --force
#klab server  & for x in examples/*; do \
#                   cd $x && klab run --headless --force
#                   cd ../..
#               done
kill $(pgrep klab)
