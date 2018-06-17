## KLab
A symbolic execution engine for EVM and explorer for K-framework reachability proofs.

**This is an early alpha version for internal usage. Direct any questions in an issue or at  https://dapphub.chat/channel/k-framework .

## Requirements
You will need a modified version of evm-semantics, containing a modified version of k:
* https://github.com/dapphub/evm-semantics use the `klab` branch

Install it with:
```
make deps
make
```

## SetUp

```
git clone git@github.com:dapphub/kdebug.git
cd kdebug
make
```

Export a path variable (e.g. save them in `~/.profiles`) pointing to the evm-semantics directory:
```
export KLAB_EVMS_PATH=/path/to/evm-semantics
```
If you want to use a custom version of K you can use the `KLAB_K_PATH` variable:
```
export KLAB_K_PATH=/path/to/k
```


You also need to set the temporary directory to use, for example:
```
export TMPDIR=/tmp
```

## Usage
Write a `spec.ini` file with the property you want to prove and put it in a directory together
with the solidity source code file together with the corresponding `.sol.json`.
From the same directory, run:

`klab run`
 
 to start the klab interactive tool.
 
[Example usage](../master/examples)
