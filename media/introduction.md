---
title: Introduction to KLab
subtitle: rmul example
date: '\today'
theme: metropolis
fontsize: 10pt
---

Motivation
==========

Using [KEVM]
------------

### Smart Contract Verification

-   Using [KFramework] prover and [KEVM] to do smart contract verification.
-   Currently [KFramework] prover provides little feedback about why proof fails.

. . .

### Enter KLab

-   Interactively explore execution traces produced by K prover.
-   Display information about each state.

Example: Fixed Point Exponentiation
===================================

Overview
--------

-   In repository <https://github.com/dapphub/k-ds-rpow>.
-   Want to do safe fixed-point exponentiation.
-   State roll-back on overflow.

Solidity Implementation
-----------------------

\tiny

```solidity
pragma solidity ^0.4.21;
contract DsRpow {

    /*
     * Fixed-point exponentiation by squaring with fixed-point base as parameter.
     *
     * Overflow-safe multiplication and addition are inlined as an optimization.
     */

    function rpow(uint x, uint n, uint base) public pure returns (uint z) {
        assembly {
      switch x case 0 {switch n case 0 {z := base} default {z := 0}}
      default {
        switch mod(n, 2)
             case 0 { z := base }
             default { z := x }
         let half := div(base, 2) // Used for rounding.
                for { n := div(n, 2) } n { n := div(n,2) } {
            let xx := mul(x, x)
                if iszero(eq(div(xx, x), x)) { revert(0,0) }
            let xxRound := add(xx, half) if lt(xxRound, xx) { revert(0,0) }
            x := div(xxRound, base)
               if mod(n,2) {
                       let zx := mul(z, x)
                   if and(iszero(iszero(x)), iszero(eq(div(zx, x), z))) { revert(0,0) }
                   let zxRound := add(zx, half) if lt(zxRound, zx) { revert(0,0) }
               z := div(zxRound, base)
                   }
                }
          }
        }
    }
}
```

\normalsize

K Spec
------

\tiny

```kini
[loop]
k: #execute => #execute
output: _
memoryUsed: 0
callData: _
wordStack: _ : _ : Half : _ : Z : Base : N : X : WS => Half : _ : #rpow(Z, X, N, Base) : Base : 0 : _ : WS
pc: 249 => 317
gas: G => _
log: _
refund: _
storage: _
calldepth: Calldepth
statuscode: _
activeaccounts:
accounts:
requires:
    andBool #sizeWordStack(WS) <Int 10
    andBool #sizeWordStack(WS) >Int 2
    andBool 0 <=Int Z andBool Z <Int pow256
    andBool 0 <=Int Half andBool Half <Int pow256
    andBool Half ==Int Base /Int 2
    andBool 0 <=Int XX andBool XX <Int pow256
    andBool 1 <=Int Base andBool Base <Int pow256
    andBool 1 <=Int N andBool N <Int pow256
    andBool 0 <=Int X andBool X <Int pow256
    andBool 0 <=Int #rpow(Z, X, N, Base) // *Int Base
    andBool #rpow(Z, X, N, Base) *Int Base <Int pow256
    andBool G >=Int (100000000 *Int N) +Int 1000000000
    andBool 0 <=Int Calldepth andBool Calldepth <Int 1024
ensures:
attribute:
```

\normalsize

Resources
---------

-   [KEVM]
-   [KFramework]

[KEVM]: <https://github.com/kframework/evm-semantics>
[KFramework]: <https://github.com/kframework/k>
