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
make deps
make
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

## Usage

Fire up a klab server instance by running `klab server` in a terminal.
Write a `spec.ini` reachability proof and put it in a directory together with the correponding solidity source code in a `.sol.json` file.
From the same directory, run:

```sh
klab run
```

to connect to the server and start the klab interactive tool.

To begin with, you can try out an example spec at [Example usage](../master/examples).
