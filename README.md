## KLab

GUI for exploring K-framework reachability proofs.

**NOTE:** This is an early alpha version for internal usage.
Direct any questions in an issue or at <https://dapphub.chat/channel/k-framework>.

## Requirements

You will need a modified version of evm-semantics, containing a modified version of k:

-   <https://github.com/dapphub/evm-semantics> use the `klab` branch

Install it with (you will need to install the KEVM dependencies listed in the `evm-semantics` repository):

```sh
git clone 'https://github.com/dapphub/evm-semantics'
cd evm-semantics
git checkout klab
make k-deps tangle-deps
make build-java
```

## Setup

Build and install the NodeJS package (requires having `npm`):

```sh
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

Finally, export the path to the `klab` executable:

```sh
export PATH=$PATH:/path/to/klab/bin
```

## Example Usage

Fire up a klab server instance by running `klab server` in a terminal.
This can be done from any directory, but the above `*PATH` variables must be set.

Within a proof directory (with a `spec.ini` file), you can run `klab run` to connect to the server and request a proof.

For example:

```sh
cd examples/SafeAdd
klab run
```

to connect to the server and start the klab interactive tool.
