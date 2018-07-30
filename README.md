KLab
====

GUI for exploring K-framework reachability proofs.

**NOTE:** This is an early alpha version for internal usage.
Direct any questions in an issue or at <https://dapphub.chat/channel/k-framework>.

KLab is a client-server architecture, meaning that you need to have a KLab server and a KLab client running.
The server will recieve proof requests and perform them, while the client will let you explore/work on the proof.
Ask at <https://dapphub.chat/channel/k-framework> for access to a KLab server if you do not want to setup your own.

Setting up KLab Server and Client
---------------------------------

### Dependencies

-   All the dependencies for the [KEVM](https://github.com/kframework/evm-semantics), excluding the Ocaml/Opam dependencies.
-   `npm` for installing the JavaScript dependencies.

Run (to install npm dependencies and KEVM):

-   `make deps-npm`: Install npm dependencies.
-   `make deps-kevm`: Clone and build KEVM semantics (requires have KEVM dependencies setup).
-   `make deps`: do both.

### Environment Setup

Your should tell KLab where the `klab` executable lives, e.g.:

```sh
export PATH=$PATH:/path/to/klab/bin
```

For running a `klab server`, you need to additionally set:

```sh
export KLAB_EVMS_PATH=/path/to/evm-semantics
export TMPDIR=/tmp
```

**OPTIONAL**: If you want to use a custom version of K you can also do:

```sh
export KLAB_K_PATH=/path/to/k
```

### Installing Globally

To make klab available from the terminal, either export the path to the `klab` executable.
Also provided is the following command:

```sh
make link
```

This installs symlinks globally at `/usr/local/bin` and `/usr/local/libexec` (will require `sudo` access on Linux machines).

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
