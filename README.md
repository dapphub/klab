KLab
====
**NOTE:** This is a beta version for internal usage.
Direct any questions in an issue or at <https://dapphub.chat/channel/k-framework>.

Klab is a tool for generating and debugging K-framework reachability proofs, tailored for formal verification of ethereum smart contracts.

It uses a custom, compact format for expressing the behavior of an ethereum contract, from which it generates K framework reachability rules. The klab server invokes the K framework prover while storing intermediate proof steps, allowing the proof object to be explored in an interactive way.

KLab uses a client-server architecture, meaning that you'll need to have both a KLab server and a KLab client running.
The server will recieve proof requests and dispatch them to K frameworks proving tool, while the client builds and explores proofs.
Ask at <https://dapphub.chat/channel/k-framework> for access to a KLab server if you do not want to setup your own.

Setting up KLab Server and Client
---------------------------------

### Dependencies

-   All the dependencies for the [KEVM](https://github.com/kframework/evm-semantics), excluding the Ocaml/Opam dependencies. Follow the [instructions](https://github.com/dapphub/evm-semantics#system-dependencies) under the section "System Dependencies" in the `evm-semantics` repo, but don't install `evm-semantics` or `k` itself: the correct versions will be automatically installed if you follow the instructions below.
-   `npm` for installing the JavaScript dependencies.

To install the remaining dependencies (the npm dependencies and KEVM):

```sh
make deps
```

### Installing

To make klab available from the terminal, you can either just export the path to the `klab` executable in `bin/`, or use:

```sh
make link
```

This installs symlinks globally at `/usr/local/bin` and `/usr/local/libexec` (will require `sudo` access on Linux machines). You can also specify a custom directory for installation by doing:

```sh
PREFIX=/path/to/custom/prefix make link
```

### Environment Setup

The file `env` will setup the environment for you if sourced from the root directory of the repo.

```sh
source env
```

It sets three environment variables:

-   `PATH`: include the `klab` executable.
-   `KLAB_EVMS_PATH`: the EVM semantics to use.
-   `TMPDIR`: temporary directory for KLab proof cacheing.

**OPTIONAL**: If you want to use a different version of K than what the KEVM ships with, you can set:

-   `KLAB_K_PATH`: override implementation of K.

Running KLab
------------

### Run Server

To start the KLab server, run:

```sh
klab server
```

### Building proofs

The KLab client is run in a proof directory, and will request that the KLab server execute the proof. Proof claims are expressed in our custom, succint [specification](#specification-format). Run `klab build` in the directory of your specification to generate K reachability rules from the specification:
```sh
cd examples/SafeAdd
klab build
```
This will generate a fail and success reachability rule for each `act` of your specification in the `SafeAdd/out/specs` directory.

### Running proofs
To explore a proof with the interactive klab GUI, use `klab run`, specifying which proof to explore using the `--spec` flag:

```sh
klab run --spec out/specs/proof-SafeAdd_add_succ.k
```

To ensure that a cached version of the proof is not being used, run `klab` with the `--force` option.

### Specification format
Klab uses a custom, concise way of specifying the behavior of an ethereum smart contract function. These are expressed in what we call `acts`. An `act` specifies which function is being called, the typing of the relevant variables involved, specifies the return data and how the contracts storage will be updated, and the conditions for success. From such an act, two proof claims are being generated: the `success` case, which assumes the conditions for success and asserts that the execution will end successfully in the asserted state, and one `fail` case, which assumes the negation of the succcess conditions and asserts that the execution will end in a `REVERT`. Below is an example of an `act`, taken from [the formal verification of multicollateral dai](https://github.com/dapphub/k-dss).
```
behaviour heal of Vat
interface heal(bytes32 u, bytes32 v, int256 rad)

types

    Can   : uint256
    Dai_v : uint256
    Sin_u : uint256
    Debt  : uint256
    Vice  : uint256

storage

    #Vat.wards(CALLER_ID) |-> Can
    #Vat.dai(v)           |-> Dai_v => Dai_v - rad
    #Vat.sin(u)           |-> Sin_u => Sin_u - rad
    #Vat.debt             |-> Debt  => Debt - rad
    #Vat.vice             |-> Vice  => Vice - rad

iff

    Can == 1

iff in range uint256

    Dai_v - rad
    Sin_u - rad
    Debt - rad
    Vice - rad
```
The interesting part of this particular function happens under the `storage` header. The meaning of the line:
`#Vat.dai(v)           |-> Dai_v => Dai_v - rad`
is that in the `success` case, the value at the storage location which we call `#Vat.dai(v)` will be updated from `Dai_v` to `Dai_v - rad`.

To prove this reachability claim, the k prover explores all possible execution paths starting from the precondition (whats on the left hand side of a `=>`) and the claim is proven if they all end in a state satisfying the postcondition (right hand side of the `=>`). 

More information about how the k prover and the k framework in general works can be found at <http://fsl.cs.illinois.edu/FSL/papers/2016/stefanescu-park-yuwen-li-rosu-2016-oopsla/stefanescu-park-yuwen-li-rosu-2016-oopsla-public.pdf> and a detailed description of the semantics of EVM defined in K is given in <https://www.ideals.illinois.edu/handle/2142/97207>

### Key Bindings

Toggle different views by pressing any of the following keys:

**View Commands**:

-   `c` - display current **c**onstraints.
-   `k` - display `<k>` cell.
-   `b` - display **b**ehavior tree.
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


Troubleshooting
---------------

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
klab run
```

# License
All contributions to this repository are licensed under AGPL-3.0. Authors:

* Denis Erfurt
* Martin Lundfall
* Everett Hildenbrandt
* Lev Livnev
