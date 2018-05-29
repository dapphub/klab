## Kdebug
modified K-framework debugger.

**This is a hacky version for internal usage, if you have questions ask @denis at dapphub.chat**

## Requirements
You will need modified versions of evm-semantics, which shps with the modiefied k:
* https://github.com/dapphub/evm-semantics

Install them with:
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
Create a working directory e.g. `~/wd`

Export path variables e.g. save them in `~/.profiles`
```
export KLAB_WD_PATH=/path/to/wd
export KLAB_K_PATH=/path/to/k
export KLAB_EVMS_PATH=/path/to/evm-semantics
```

## Usage
go your project directory and run:

`klab run --no-repeat`
