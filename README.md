KLab
====

GUI for exploring K-framework reachability proofs.

**NOTE:** This is an early alpha version for internal usage.
Direct any questions in an issue or at <https://dapphub.chat/channel/k-framework>.

KLab is a client-server architecture, meaning that you need to have a KLab server and a KLab client running.
The server will recieve proof requests and perform them, while the client will let you explore/work on the proof.
Ask at <https://dapphub.chat/channel/k-framework> for access to a KLab server if you do not want to setup your own.

KLab Client
-----------

The client simple allows exploring a proof that the server has generated.

### Dependencies

-   `npm` for installing the JavaScript dependencies.

Run (to install npm dependencies):

```sh
make deps-npm
```

### Environment Setup

Your should tell KLab where the `klab` executable lives, e.g.:

```sh
export PATH=$PATH:/path/to/klab/bin
```

### Running

Within a proof directory (with a `spec.ini` file), you can run `klab run` to connect to the server and request a proof.
The server location is set in the proof-directory's `config.json` file, where the default is `127.0.0.1`.

For example:

```sh
cd examples/SafeAdd
klab run
```

To ensure that a cached version of the proof is not being used, you need to first:

-   Remove the temporary directory on the server.
-   Run `klab` with the `--force` option.

```sh
rm -rf /path/to/proof/dir/on/server
klab run --force
```

Toggle different views by pressing any of the following keys:

**View Commands**:

-   `k` - display the `<k>` cell.
-   `b` - display **b**ehavior
-   `e` - display the **e**vm specific module.

**Navigation Commands**:

-   `n` - step to **n**ext opcode
-   `p` - step to **p**revious opcode
-   `N` - step to **n**ext k term
-   `P` - step to **p**revious k term

See file [examples/SafeAdd/config.json] for more example movement commands.

KLab Server
-----------

In addition to the install instructions for KLab Client, do the following.

### Dependencies

First install all the dependencies for the [KEVM](https://github.com/kframework/evm-semantics), excluding the Ocaml/Opam dependencies.

Run (to clone KEVM submodule and build it):

```sh
make deps-kevm
```

### Environment Setup

The following extra environment variables should be set:

```sh
export KLAB_EVMS_PATH=/path/to/evm-semantics
export TMPDIR=/tmp
```

**OPTIONAL**: If you want to use a custom version of K you can also do:

```sh
export KLAB_K_PATH=/path/to/k
```

### Running

To start the KLab server, run:

```sh
klab server
```

Installing Globally
-------------------

To make klab available from the terminal, either export the path to the `klab` executable.
Also provided is the following command:

```sh
make link
```

This installs symlinks globally at `/usr/local/bin` and `/usr/local/libexec` (will require `sudo` access on Linux machines).
