---
title: Introduction to KLab
subtitle: SafeAdd example
date: '\today'
theme: metropolis
fontsize: 10pt
---

Motivation
----------

### Smart Contract Verification

-   Using [KEVM] to do smart contract verification.
-   Current [KFramework] prover provides little feedback failing proofs.

. . .

### Enter KLab

-   Interactively explore execution traces produced by K prover.
-   Display information about each state.
-   Get support on the [DappHub Chat] at <https://dapphub.chat>.

Example: Safe Addition
======================

Overview
--------

-   In directory `examples/SafeAdd`.

. . .

-   If the addition does not overflow, performs regular addition.
-   On overflow, should `REVERT`.

Solidity (file: `examples/SafeAdd/SafeAdd.sol`)
-----------------------------------------------


```solidity
pragma solidity ^0.4.21;
contract SafeAdd {
    function add(uint x, uint y) public pure
      returns (uint z) {
        require((z = x + y) >= x);
    }
}
```

K Spec (file: `examples/SafeAdd/spec.ini`)
------------------------------------------

\footnotesize

```kini
[add]
k: #execute => #halt
statusCode: _ => EVMC_SUCCESS
output: _ => #asByteStackInWidth(X +Int Y, 32)
callData: #abiCallData("add", #uint256(X), #uint256(Y))
localMem: _
pc: 0 => _
wordStack: .WordStack => _
gas: G => _
log: _
callDepth: _
refund: _
storage: _
activeaccounts:
accounts:
requires:
    andBool #rangeUInt(256, X)
    andBool #rangeUInt(256, Y)
    andBool G >Int 330
    andBool #rangeUInt(256, X +Int Y)
```

\normalsize

Steps
-----

1.  Open README.md to follow install instructions for KLab.
2.  Setup environment variables for the KLab server and the KLab client.
3.  Start KLab server (`klab server`).
4.  In directory `examples/SafeAdd`, change the spec to not pass.
5.  Start the KLab client (`klab run --force`).
6.  Explore the behaviour, see why the spec does not pass.
7.  Fix the spec, re-run KLab client.

Resources
---------

-   [DappHub] - Developers of KLab
-   [DappHub Chat] - Technical support for KLab
-   [KEVM] - Semantics of EVM in K
-   [KLab] - This tool
-   [KFramework] - Semantic framework for programming languages
-   [Runtime Verification] - Developers of K

[DappHub]: <https://dapphub.com/>
[DappHub Chat]: <https://dapphub.chat>
[KEVM]: <https://github.com/kframework/evm-semantics>
[KLab]: <https://github.com/dapphub/klab>
[KFramework]: <https://github.com/kframework/k>
[Runtime Verification]: <https://runtimeverification.com>
