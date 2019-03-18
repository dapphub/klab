FROM lnl7/nix:2.2

RUN nix-channel --add https://nixos.org/channels/nixos-19.03 nixpkgs
RUN nix-channel --update

WORKDIR /klab

# install nix deps
COPY shell.nix /klab/shell.nix
RUN nix-shell /klab/shell.nix --command ""

# download maven artifacts
COPY evm-semantics/.build/k/pom.xml /klab/pom.xml
RUN nix-shell /klab/shell.nix --command "mvn verify --fail-never"

# build source
COPY . .
RUN nix-shell /klab/shell.nix --command "make deps"
RUN nix-shell /klab/shell.nix --command "make haskell-deps"

CMD nix-shell /klab/shell.nix
