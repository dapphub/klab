#!/usr/bin/env bash
<<<<<<< HEAD
K_OPTS=-Xmx10G \
$KLAB_EVMS_PATH/.build/k/k-distribution/target/release/k/bin/kprove \
--debugg \
--debugg-path $TMPDIR/klab \
--debugg-id $2 \
--directory $KLAB_EVMS_PATH/.build/java/ \
--z3-executable \
--def-module RULES \
--output-omit "<programBytes> <program> <code> <previousGas> <touchedAccounts> <interimStates> <callStack> <callData>" \
--output-flatten "_Map_ #And" \
--output json \
--smt_prelude ./prelude.smt2 \
--z3-tactic "(or-else (using-params smt :random-seed 3 :timeout 1000) (using-params smt :random-seed 2 :timeout 2000) (using-params smt :random-seed 1))" \
$1
=======
K_OPTS=-Xmx10G $KLAB_EVMS_PATH/.build/k/k-distribution/target/release/k/bin/kprove \
    --debugg --debugg-path $TMPDIR/klab --debugg-id $2 \
    --directory $KLAB_EVMS_PATH/.build/java/ --def-module RULES \
    --z3-executable --smt_prelude ./prelude.smt2 --z3-tactic "(or-else (using-params smt :random-seed 3 :timeout 1000) (using-params smt :random-seed 2 :timeout 2000) (using-params smt :random-seed 1))" $1 \
    --output json \
    --output-tokenize "#And _==K_ <k> #unsigned" \
    --output-omit "<programBytes> <program> <code> <previousGas> <touchedAccounts> <interimStates> <callStack>" \
    --output-flatten "_Map_ #And"
>>>>>>> resources/run.sh: newlines for readability
