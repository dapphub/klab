## KLab

GUI for exploring K-framework reachability proofs.

**NOTE:** This is an early alpha version for internal usage.
Direct any questions in an issue or at <https://dapphub.chat/channel/k-framework>.

## Dependencies

-   All the dependencies for the [KEVM](https://github.com/kframework/evm-semantics) (excluding the OCaml/Opam dependencies).
-   `npm` for installing the JavaScript dependencies.

## Setup KEVM

To setup dependencies, run:

-   `make deps-kevm`: initialize KEVM submodule and build it.
-   `make deps-npm`: install JavaScript dependencies.
-   `make deps`: do both.

## Environment Setup

The following environment variables should be set (e.g. save them in `~/.profiles`):

```sh
export KLAB_EVMS_PATH=/path/to/evm-semantics
export PATH=$PATH:/path/to/klab/bin
export TMPDIR=/tmp
```

If you used the above `make deps`, you can do (from the root of the repository):

```sh
export KLAB_EVMS_PATH=$(pwd)/evm-semantics
export PATH=$PATH:$(pwd)/bin
export TMPDIR=/tmp
```

**OPTIONAL**: If you want to use a custom version of K you can also do:

```sh
export KLAB_K_PATH=/path/to/k
```

To make klab available from the terminal, either export the path to the `klab` executable:

or simply run 

```sh
make link
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

To ensure that a cached version of the proof is not being used, do:

```sh
klab run --force
```

## Usage

Toggle different views by pressing any of the following keys:

* `k` - display the `<k>` cell.
* `N` - step to **n**ext k term
* `n` - step to **n**ext branching point
* `P` - step to **p**revious k term
* `p` - step to **p**revious branching point
* `b` - display **b**ehavior
* `h` - display **h**elp (TODO)

See file [examples/SafeAdd/config.json] for more example movement commands.
