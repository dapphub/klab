## Kdebug
modified K-framework debugger.

**This is a hacky version for internal usage, if you have questions ask @denis at dapphub.chat**

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

Export path variables e.g. save them in `~/.profiles`
```
export KLAB_K_PATH=/path/to/k
export KLAB_EVMS_PATH=/path/to/evm-semantics
```

## Usage
Write a `spec.ini` file with the property you want to prove and put it in a directory together
with the solidity source code file together with the corresponding `.sol.json`.
From the same directory, run:

`klab run`
 
 to start the klab interactive tool.
 
[Example usage](../blob/master/examples)
