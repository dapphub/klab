#!/usr/bin/env bash
KLAB_EVMS_PATH=$(pwd)/evm-semantics
klab server  & cd examples/SafeAdd && klab run --headless --force
kill $(pgrep klab)
