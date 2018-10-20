KLab
====
**NOTE:** This software is still in the early stages of development. If you are confused, find some bugs, or just want some help, please file an issue or come talk to us at <https://dapphub.chat/channel/k-framework>.

Klab is a tool for generating and debugging K-framework reachability proofs, tailored for formal verification of ethereum smart contracts.

It uses a custom, compact format for expressing the behavior of an ethereum contract, from which it generates K framework reachability rules. The klab server invokes the K framework prover while storing intermediate proof steps, allowing the proof object to be explored in an interactive way.

KLab uses a client-server architecture, meaning that you'll need to have both a KLab server and a KLab client running.
The server will recieve proof requests and dispatch them to K frameworks proving tool, while the client builds and explores proofs.
Ask at <https://dapphub.chat/channel/k-framework> for access to a KLab server if you do not want to setup your own.

Setting up KLab Server and Client
---------------------------------

One option is to use Docker:

```sh
docker run -it dapphub/klab
```

[See below](#docker) for details on using Docker.

### Dependencies
Installing klab automatically installs `K` and `KEVM`. You will therefore need the dependencies of K.

To install all of these dependencies on Ubuntu, try:

```sh
sudo apt-get install make gcc maven openjdk-8-jdk flex pkg-config libmpfr-dev autoconf libtool pandoc zlib1g-dev z3 libz3-dev npm
```

On ArchLinux:

```sh
sudo pacman -S  base-devel rsync opam pandoc jre8-openjdk mpfr maven z3 nodejs npm
```

On OSX, using Homebrew, after installing the command line tools package:

```sh
brew tap caskroom/cask caskroom/versions
brew cask install caskroom/versions/java8
brew install automake libtool gmp mpfr pkg-config pandoc maven opam z3 node
```

### Installing
Clone the repo and install the latest stable version `v0.2.3` with
```sh
git clone --branch v0.2.3 https://github.com/dapphub/klab.git
cd klab
make deps
```

To make klab available from the terminal, you can either just export the path to the `klab` executable in `bin/`, or use:

```sh
make link
```

This installs symlinks globally at `/usr/local/bin` and `/usr/local/libexec` (will require `sudo` on Linux machines). You can also specify a custom directory for installation by doing:

```sh
PREFIX=/path/to/custom/prefix make link
```

*OPTIONAL*: `klab` has some optional Haskell components, for which the recommended installation method is [nix](https://nixos.org/nix/). If you have `nix`, you can install the Haskell components with

```sh
make deps-haskell
```

### Environment Setup

You may wish to add the `klab` executable to the path, e.g.:

```sh
export PATH=$PATH:/path/to/klab/bin
```

Prior to running `klab`, make sure the following environment variables are set:

```sh
export KLAB_EVMS_PATH=/path/to/evm-semantics
export TMPDIR=/tmp
```

The `evm-semantics` are located in this repo, e.g. if you cloned this repo to `/home/foo/repos/klab`, you should run:

```sh
export KLAB_EVMS_PATH=/home/foo/repos/klab/evm-semantics
```

**OPTIONAL**: If you want to use a custom version of K you can also set:

```sh
export KLAB_K_PATH=/path/to/k
```

Running KLab
------------

### Run Server

To start the KLab server, run:

```sh
klab server
```

### Building proofs

The KLab client is run in a proof directory, and will request that the KLab server execute the proof. Proof claims are expressed in a custom, succint [specification language](acts.md), from which K reachability rules are generated using `klab build`. In order to generate reachability claims, you need to provide a `.sol.json` generated from the contract you want to verify. With [solc](https://solidity.readthedocs.io/en/latest/installing-solidity.html) installed, you can generate this file by running
```sh
solc --combined-json=abi,bin,bin-runtime,srcmap,srcmap-runtime,ast <path-to-contract> > <path-to-output>
```
You need to tell `klab` where to find the `sol.json` file by specifying it under the `implementations` header in your projects `config.json`. Consult the [examples](examples) for more information. With your config pointing to the outputted evm binaries, you can run `klab build` in the same direcory:
```sh
cd examples/SafeAdd
klab build
```
This will generate a fail and success reachability rule for each `act` of your specification in the `SafeAdd/out/specs` directory.

### Running proofs

To explore a proof with the interactive klab GUI, use `klab debug`, specifying which proof to explore:

```sh
klab debug out/specs/SafeAdd_add_pass.k
```

To ensure that a cached version of the proof is not being used, run `klab` with the `--force` option.

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

### Docker

Example usage:
```sh
# Build server
docker build -t klab .

# Start server and mount ./examples to /docker
docker run --rm -it -v $(pwd)/examples:/docker --name klab klab
klab server

# Start client
docker exec -it klab bash
cd /docker/SafeAdd
klab debug
```

# License
All contributions to this repository are licensed under AGPL-3.0. Authors:

* Denis Erfurt
* Martin Lundfall
* Everett Hildenbrandt
* Lev Livnev
