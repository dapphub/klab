KLab
====

A GUI for exploring K-framework reachability proofs.

**NOTE:** This is an early alpha version for internal usage.
Direct any questions in an issue or at <https://dapphub.chat/channel/k-framework>.

KLab uses a client-server architecture, meaning that you'll need to have both a KLab server and a KLab client running.
The server will recieve proof requests and perform them, while the client will let you explore/work on the proof.
Ask at <https://dapphub.chat/channel/k-framework> for access to a KLab server if you do not want to setup your own.

Setting up KLab Server and Client
---------------------------------

### Dependencies

-   All the dependencies for the [KEVM](https://github.com/kframework/evm-semantics), excluding the Ocaml/Opam dependencies.
-   `npm` for installing the JavaScript dependencies.

Run (to install npm dependencies and KEVM):

-   `make deps-npm`: Install npm dependencies.
-   `make deps-kevm`: Clone and build KEVM semantics (requires having KEVM dependencies setup).
-   `make deps`: Do both (default when running `make`)

### Installing

To make klab available from the terminal, you can either just export the path to the `klab` executable, as explained in [Environment Setup](#environment-setup), or use:

```sh
make link
```

This installs symlinks globally at `/usr/local/bin` and `/usr/local/libexec` (will require `sudo` access on Linux machines). You can also specify a custom directory for installation by doing:

```sh
PREFIX=/path/to/custom/prefix make link
```

### Environment Setup

You may wish to add the `klab` executable to the path, e.g.:

```sh
export PATH=$PATH:/path/to/klab/bin
```

To run a `klab server`, you need to additionally set:

```sh
export KLAB_EVMS_PATH=/path/to/evm-semantics
export TMPDIR=/tmp
```

**OPTIONAL**: If you want to use a custom version of K you can also do:

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

### Run Client

The KLab client is run in a proof directory, and will request that the KLab server execute the proof.
From within a proof directory (with a `spec.ini` file), you can run `klab run` to connect to the server and request a proof.
The server location is set in the proof-directory's `config.json` file, where the default is `127.0.0.1`.

For example:

```sh
cd examples/SafeAdd
klab run
```

To ensure that a cached version of the proof is not being used, you need to first:

-   Remove the temporary directory on the server (printed out in the server log).
-   Run `klab` with the `--force` option.

```sh
rm -rf /path/to/proof/dir/on/server
klab run --force
```

### Key Bindings

Toggle different views by pressing any of the following keys:

**View Commands**:

-   `k` - display the `<k>` cell.
-   `b` - display **b**ehavior
-   `e` - display the **e**vm specific module.
-   `d` - display the **d**ebug cells (see toggling debug cells below).

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

See file [config example for SafeAdd](examples/SafeAdd/config.json) for more example movement commands.

### Multiproof support (experimental)

If you want to use another spec as a trusted rewrite for your proof, you can supply their .ini spec along with the binaries they relate to using the `--trust` flag:
```sh
klab run --trust myLemma.ini,myLemmasBinaries
```
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
```shell
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
