FROM lnl7/nix:2.2

WORKDIR /klab

# install nix deps
COPY shell.nix /klab/shell.nix
RUN nix-shell /klab/shell.nix --command ""

# build source
COPY . .
RUN nix-shell /klab/shell.nix --command "make deps"
RUN nix-shell /klab/shell.nix --command "make deps-haskell"

CMD nix-shell /klab/shell.nix
