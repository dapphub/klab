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

Example: Safe Addition
======================

Overview
--------

-   At directory `examples/SafeAdd`.
-   Should `REVERT` if the addition overflows.

Solidity Implementation
-----------------------

File: `examples/SafeAdd/SafeAdd.sol`

```solidity
pragma solidity ^0.4.21;
contract SafeAdd {
    function add(uint x, uint y) public pure
      returns (uint z) {
        require((z = x + y) >= x);
    }
}
```

K Spec
------

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

Resources
---------

-   [DappHub] - Developers of KLab
-   [KEVM] - Semantics of EVM in K
-   [KLab] - This tool
-   [KFramework] - Semantic framework for programming languages
-   [Runtime Verification] - Developers of K

[DappHub]: <https://dapphub.com/>
[KEVM]: <https://github.com/kframework/evm-semantics>
[KLab]: <https://github.com/dapphub/klab>
[KFramework]: <https://github.com/kframework/k>
[Runtime Verification]: <https://runtimeverification.com>
