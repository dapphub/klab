KLab
====
**NOTE:** This software is still in the early stages of development. If you are confused, find some bugs, or just want some help, please file an issue or come talk to us at <https://dapphub.chat/channel/k-framework>.

Klab is a tool for generating and debugging proofs in the [K Framework](http://www.kframework.org/index.php/Main_Page), tailored for the formal verification of ethereum smart contracts. It includes a succinct [specification language](acts.md) for expressing the behavior of ethereum contracts, and an interactive debugger.

Installation
------------

### Dependencies

[See dependency installation instructions here](https://github.com/kframework/evm-semantics#system-dependencies)

This project uses the `GNU` version of `getopt` and `time`. `OSX` and `gnu` have a complicated relationship but you can run:
```sh
export PATH=/usr/local/opt/gnu-getopt/bin:/usr/local/opt/gnu-time/libexec/gnubin:$PATH
```
to make them get along.

### Building

Clone the repo and install the latest stable version `0.3` with
```sh
git clone --branch 0.3 https://github.com/dapphub/klab.git
cd klab
make deps
```

**OPTIONAL**: `klab` has some optional Haskell components, for which the recommended installation method is [nix](https://nixos.org/nix/). If you have `nix`, you can install the Haskell components with

```sh
make deps-haskell
```

### Environment Setup

To make klab available from the terminal, you can either just export the path to the `klab` executable in `bin/`, or use:

```sh
make link
```

This installs symlinks globally at `/usr/local/bin` and `/usr/local/libexec` (will require `sudo` on Linux machines). You can also specify a custom directory for installation by doing:

```sh
PREFIX=/path/to/custom/prefix make link
```
The file `env` will setup the environment for you if sourced from the root directory of the repo.

```sh
source env
```

It sets three environment variables:

-   `PATH`: include the `klab` executable.
-   `KLAB_EVMS_PATH`: the EVM semantics to use.

**OPTIONAL**: If you want to use a different version of K than what the KEVM ships with, you can set:

-   `KLAB_K_PATH`: override implementation of K.

**OPTIONAL**: You might also want to add the K tool binaries in `evm-semantics/.build/k/k-distribution/bin` to your `$PATH`, if you didn't already have K installed.

**OPTIONAL**: You can also use `nix-shell` for a more deterministic environment experience. If you have `nix` installed, run `nix-shell` in this repo to start a deterministic shell environment.

Usage
-----

To see how klab is used, we can explore the project in `examples/SafeAdd`:
```sh
cd examples/SafeAdd/
```

### Specification

The file `config.json` tells klab where to look for both the specification and the implementation of our contract. In this case, our specification lives in `src/`, and our implementation lives in `dapp/`.

Note that this example includes `dapp/out/SafeAdd.sol.json` compiled from the solidity source. With [solc](https://solidity.readthedocs.io/en/latest/installing-solidity.html) installed, you can compile it yourself:
```sh
solc --combined-json=abi,bin,bin-runtime,srcmap,srcmap-runtime,ast dapp/src/SafeAdd.sol > dapp/out/SafeAdd.sol.json
```

### Proof

Our goal is to prove that our implementation satisfies our specification. To do so, we'll start by building a set of K modules from our spec:
```sh
klab build
```
This will generate success and failure reachability rules for each `act` of our specification. We can find the results in the `out/specs` directory.

Now we're ready to prove each case, for example:
```sh
klab prove --dump SafeAdd_add_fail
```
The `--dump` flag outputs a log to `out/data/<hash>.log`, which will be needed later for interactive debugging. We can also do `klab prove-all` to prove all outstanding claims.

Once the proof is complete, we can explore the generated symbolic execution trace using:
```sh
klab debug <hash>
```

### Key Bindings

Toggle different views by pressing any of the following keys:

**View Commands**:

-   `t` - display the (somewhat) pretty K **t**erm.
-   `c` - display current **c**onstraints.
-   `k` - display `<k>` cell.
-   `b` - display **b**ehavior tree.
-   `s` - diaplay **s**ource code.
-   `e` - display **e**vm specific module.
-   `m` - display **m**emory cell.
-   `d` - display **d**ebug cells (see toggling debug cells below).
-   `r` - display applied K **r**ule.
-   `z` - display **z**3 feedback from attempted rule application.
-   `Up/Dn` - scroll view **up** and **down**.

**Navigation Commands**:

-   `n`       - step to **n**ext opcode
-   `p`       - step to **p**revious opcode
-   `Shift+n` - step to **n**ext k term
-   `Shift+p` - step to **p**revious k term
-   `Ctrl+n`  - step to **n**ext branch point
-   `Ctrl+p`  - step to **p**revious branch point

**Toggling Debug Cells**:

The following commands are prefixed with `:` (and are typed at the bottom of the interface).
It's possible to toggle the debug cells view for specific cells, which prints out the JSON representation of the given cells.
Remember, you must turn on the **d**ebug cells view to see these (above).

-   `:show ethereum.evm.callState.gas` - show the contents of the `<gas>` cell in the **d**ebug cells view.
-   `:hide ethereum.evm.callStack.pc`  - hide the contents of the `<pc>` cell in the **d**ebug cells view.
-   `:omit   gas pc` - omit the contents of the `<gas>` and `<pc>` cells in the **t**erm view.
-   `:unomit pc programBytes`  - unomit the contents of the `<pc>` and `<programBytes>` cells in the **t**erm view.

### Available klab Commands

-   `klab build` - builds a set of K reachability claims in `out/specs` based on the spec, lemmas and source code as specified in the projects `config.json`.
-   `klab prove <hash> [--dump]` - executes a K reachability claim specified as a hash to the K prover. If the `--dump` flag is present, the proof can be explored using `klab debug`.
-   `klab prove-all` - builds and executes all proof objects in the project directory.
-   `klab debug <hash>` - opens up the cli proof explorer of a particular proof execution. See key bindings above.
-   `klab focus <hash>` - focus on a hash, allowing you to leave out it as an argument to other commands.
-   `klab hash` - prints the hash of the focused proof
-   `klab get-gas <hash>` - Traverses the execution trace of a proof object to fetch its gas usage, put in `out/gas/<hash>gas.k`.
-   `klab solve-gas <hash>` - Constructs the gas condition necessary for an execution to succeed.
-   `klab evm <hash>` - Shows opcodes and source code side-by-side (useful for extracting pc values).
-   `klab status <hash>` - Shows whether a proof has been run, and whether it was accepted or rejected.
-   `klab status-js <hash>` - Shows the behaviour tree for an executed proof.
-   `klab fetch <url>` - Fetches the execution trace of a proof object at the url, preparing it for local debugging.
-   `klab zip <hash>` - zips up an execution trace so you can share it with a friend (or enemy).
-   `klab storage <contractName>` - Guesses what the storage layout of a given contract is
-   `klab report` - Generates a html report of the current project state in `out/report/index.html`.
-   `klab help` - Generates this view


### Zsh completions

There are automatic tab completions for `zsh` that can be installed by adding the following to your `.zshrc`:

```sh
# completions for klab
fpath=(~/dapphub/klab/resources/zsh $fpath)
autoload -U compinit
compinit
```


Troubleshooting
---------------

### Outdated npm

You might have problems due to an outdated `npm`, in that case try updating it with:

```sh
npm install npm@latest -g
npm install -g n
n stable
```

### KLab server requesting files at incorrect directory

What it looks like:

```
$ klab server

18.07.30 14-46-50: exec dfc688db4cc98b5de315bdfaa2512b84d14c3aaf3e58581ae728247097ff300d/run.sh
18.07.30 14-47-32: out Debugg: dfc688db4cc98b5de315bdfaa2512b84d14c3aaf3e58581ae728247097ff300d

fs.js:119
throw err;
^

Error: ENOENT: no such file or directory, open '/tmp/klab/b042c99687ae5018744dc96107032b291e4a91f1ab38a6286b2aff9a78056665/abstract-semantics.k'
at Object.openSync (fs.js:443:3)
at Object.readFileSync (fs.js:348:35)
at getFileExcerpt (/home/dev/src/klab/lib/rule.js:5:4)
at Object.parseRule (/home/dev/src/klab/lib/rule.js:21:16)
at Object.getblob (/home/dev/src/klab/lib/driver/dbDriver.js:49:19)
at Object.next (/home/dev/src/klab/lib/driver/dbDriver.js:113:56)
at Stream._n (/home/dev/src/klab/node_modules/xstream/index.js:797:18)
at /home/dev/src/klab/node_modules/@cycle/run/lib/cjs/index.js:57:61
at process._tickCallback (internal/process/next_tick.js:61:11)
[1] [dev@arch-ehildenb klab]% klab server
fs.js:119
throw err;
```

Notice how it's requesting `abstract-semantics.k` from proof-hash `b042...` but we're actually running proof-hash `dfc6...`.
This is a problem with how K caches compiled definitions, and must be [fixed upstream](https://github.com/kframework/k/issues/107).

To fix this, run:

```sh
make clean && make deps
```

This will remove and recompile the KEVM semantics.

# License
All contributions to this repository are licensed under AGPL-3.0. Authors:

* Denis Erfurt
* Martin Lundfall
* Everett Hildenbrandt
* Lev Livnev
