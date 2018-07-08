## KLab
GUI for exploring K-framework reachability proofs.

**This is an early alpha version for internal usage. Direct any questions in an issue or at  https://dapphub.chat/channel/k-framework .

## Requirements
You will need a modified version of evm-semantics, containing a modified version of k:
* https://github.com/dapphub/evm-semantics use the `klab` branch

Install it with:
```
make deps
make
```

## Setup

```
git clone git@github.com:dapphub/kdebug.git
cd kdebug
make
```

Export a path variable (e.g. save them in `~/.profiles`) pointing to the evm-semantics directory:
```
export KLAB_EVMS_PATH=/path/to/evm-semantics
```
OPTIONAL: If you want to use a custom version of K you can use the `KLAB_K_PATH` variable:
```
export KLAB_K_PATH=/path/to/k
```

You also need to set the temporary directory to use, for example:
```
export TMPDIR=/tmp
```

## Usage
Fire up a klab server instance by running `klab server` in a terminal.
Write a `spec.ini` reachability proof and put it in a directory together
with the correponding solidity source code in a `.sol.json` file.
From the same directory, run:

`klab run`

 to connect to the server and start the klab interactive tool.

To begin with, you can try out an example spec at [Example usage](../master/examples)
