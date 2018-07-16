## KLab

GUI for exploring K-framework reachability proofs.

**NOTE:** This is an early alpha version for internal usage.
Direct any questions in an issue or at <https://dapphub.chat/channel/k-framework>.

## Dependencies

-   All the dependencies for the [KEVM](https://github.com/kframework/evm-semantics) (excluding the OCaml/Opam dependencies).
-   `npm` for installing the JavaScript dependencies.

## Setup KEVM

You will need a modified version of evm-semantics, containing a modified version of k:

-   <https://github.com/dapphub/evm-semantics> use the `dapphub/stable` branch

Install it with:

```sh
git clone 'https://github.com/dapphub/evm-semantics'
cd evm-semantics
git checkout dapphub/stable
make k-deps tangle-deps -B
make build-java -B
```

## Setup

To install klab, clone this repository and install it with `npm`:

```sh
git clone 'https://github.com/dapphub/klab
cd klab
npm install
```

Export a path variable (e.g. save them in `~/.profiles`) pointing to the evm-semantics directory:

```sh
export KLAB_EVMS_PATH=/path/to/evm-semantics
```

**OPTIONAL**: If you want to use a custom version of K you can use the `KLAB_K_PATH` variable:

```sh
export KLAB_K_PATH=/path/to/k
```

You also need to set the temporary directory to use, for example:

```sh
export TMPDIR=/tmp
```

To make klab available from the terminal, either
export the path to the `klab` executable:

```sh
export PATH=$PATH:/path/to/klab/bin
```

or simply run 

```sh
make
```
in the klab main directory.

## Get started

Fire up a klab server instance by running `klab server` in a terminal.
This can be done from any directory, but the above `*PATH` variables must be set.

Within a proof directory (with a `spec.ini` file), you can run `klab run` to connect to the server and request a proof.

For example:

```sh
cd examples/SafeAdd
klab run
```
to connect to the server and start the klab interactive tool.

## Usage

Toggle different views by pressing any of the following keys:

* `t` - display part of the k **t**erm
* `N` - step to **n**ext k term
* `n` - step to **n**ext branching point
* `P` - step to **p**revious k term
* `p` - step to **p**revious branching point
* `b` - display **b**ehavior
* `h` - display **h**elp (TODO)
