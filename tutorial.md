# A `klab` Tutorial

This documented is intended as a practical introduction to the practice of verifying smart contracts
using `klab`. No prior knowledge of formal methods or programming language theory is assumed.

This tutorial will teach you how to model smart contracts, using techniques built upon a rigorous formal
grounding, and determine whether a given configuration of `EVM` bytecode faithfully implements the model.

To put it in more practical terms, you will learn how to define tests for methods of solidity smart
contracts, that describe *all possible behaviours* of the method.

This tutorial is concerned with the hard practicalities of formal verification, not the theoretical
foundations. It is hands on and practical. You do not need to be a mathemetician to follow along.

## Why Verify Formally

Humans are bad at fully grasping the complexity of even very small software systems. Unit tests have
been proven to be very ineffective at actually finding bugs. Formal methods give us the tools we
need to ensure critical system invariants with a very high degree of confidence.

In addition the process of generating a mathematically rigourus description of a software system
nescessitates the kind of close reading and detailed attention that builds deep understanding and
itself helps to uncover bugs.

## Tooling

### `klab`

`klab` is a thin convenience wrapper around the [`K` framework](http://www.kframework.org/index.php/Main_Page), and in particular the
[`evm-semantics`](https://github.com/kframework/evm-semantics). It was developed as part of the work
on formally verifying the smart contracts for [multi collateral Dai](https://github.com/dapphub/k-dss).

`klab` has a few main functions:

- compilation of [`act`](https://github.com/dapphub/klab/blob/master/acts.md) specifications into `K` reachability claims (`klab build`)
- interactive exploration of debug traces produced by the `kprove` tool (`klab debug`)
- orchestration and reporting of proof runs (`klab prove`, `klab prove-all`, and `klab report`)

### EVM Semantics

The [EVM semantics](https://github.com/dapphub/klab/blob/master/acts.md) (a.k.a `KEVM`, Jello
Paper), is a formal semantics of the Ethereum virtual machine written in `K`.

The interpreter produced from these semantics passes the full [ethereum test
suite](https://github.com/ethereum/tests) and the semantics themselves can be viewed as the
canonical description of the EVM.

`klab` proofs are executed against the symbolic execution engine derived from these semantics.

### `K` Framework

[`K`](http://www.kframework.org/index.php/Main_Page) is a framework for defining executable formal semantics of
programming languages. Once the semantics of a given language has been defined in `K`, the framework
can automatically generate tooling for symbolic execution and program verification.

The theoretical basis of `K` is [`matching logic`](http://www.matching-logic.org/index.php/Matching_Logic).
An understanding of the theory is not required for the purposes of this tutorial.

The framework is powerful and generic. High quality semantics of
[JavaScript](https://github.com/kframework/javascript-semantics),
[C](https://github.com/kframework/c-semantics), [EVM](https://github.com/kframework/evm-semantics),
[Java](https://github.com/kframework/java-semantics), [Web
Assembly](https://github.com/kframework/wasm-semantics), and [X86-64
Assembly](https://github.com/kframework/X86-64-Semantics) have been written in `K`.

It is also slow and often inscrutable. Just parsing takes more than a minute, and even quite simple
proofs can take 10's of minutes to execute. Error messages are often unhelpful and seemingly
unrelated to the source of the problem.

## Installation

The easiest way to use `klab` is with the [`nix`](https://nixos.org/) package manager. You can
install `nix` by running:

```sh
curl https://nixos.org/nix/install | sh
```

Alternative releases and instructions for `gpg` verification are [here](https://nixos.org/nix/download.html).

Once `nix` is installed you can enter a `klab` shell by running `nix-shell` from the project root.
Once inside the `nix` shell, run:

```sh
make deps
make deps-haskell
```

This will build `klab` including all required dependencies.

You can enter this shell every time you wish to use `klab` and it will be available on your path
alongside all required runtime dependencies.

### Editor Integration

- The `K` framework provides syntax highlighting files for various editors [here](http://www.kframework.org/index.php/Editor_Support)
- A `vim` syntax highlighting file for `act` can be found at [`xwvvvvwx/act-vim`](https://github.com/xwvvvvwx/act-vim).

## Tutorial Overview

This text is itself an executable `act` spec, and can be compiled into `K` reachability claims by
running `klab build` from the root of the `tutorial` directory. It is structured as a series of
`act` specs, with surrounding literate description.

It is recomended that before continuing you open this file in a text editor on a machine with `klab` installed.

## Basic Syntax And Proof Exploration

`act` is a concise, declarative, specification DSL for ethereum smart contracts that compiles into
`K` reachability claims.

Consider the following trivial smart contract:

```solidity
contract Trivial {
  function one() payable returns uint {
    returns 1;
  }
}
```

We can model this contract with the following act specification:

```act
behaviour one of Trivial
interface one()

returns 1
```

You can build and prove this trivial spec by running the following from the tutorial root:

```
make dapp
klab build
klab focus Trivial_one_pass_rough
klab prove --dump
```

This will take a few minutes.

`act` specs are intended to be comprehensible by both humans and machines, and are therefore are
written in a literate style as embedded code blocks within markdown files. Code blocks that are not
annotated with `act` will be ignored by `klab build`.

Speaking precisely, the above `act` declares that every call to an address containing the contract
`Trivial`, where the first four bytes of calldata are the first four bytes of `keccak256("one()")`,
will always return a 256bit word with the lowest order bit set to `1`.

A full description of the `act` syntax can be found [here](https://github.com/dapphub/klab/blob/master/acts.md).
You should go skim through it while you wait for the spec to prove.

Once the proof has succeeded, run `klab debug` to start the interactive proof explorer. You will
initially be presented with a blank screen. press `e` to bring up the `evm` view. press `s` to bring
up the `solidity` view. You should see something that looks like a traditional debugger. Press `n`
to move forward one step, and `p` to move backwards.
