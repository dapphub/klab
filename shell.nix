with import (builtins.fetchGit rec {
  name = "nixpkgs-19.09-${rev}";
  url = https://github.com/nixos/nixpkgs;
  ref = "nixos-19.09";
  # git ls-remote https://github.com/nixos/nixpkgs-channels nixos-19.09
  rev = "9f453eb97ffe261ff93136757cd08b522fac83b7";
}) {};
stdenv.mkDerivation {
  name = "klab-env";
  buildInputs = [
    autoconf
    bc
    flex
    utillinux
    gcc
    getopt
    git
    gmp
    gnumake
    jq
    maven
    nix
    mpfr
    ncurses
    nodejs-10_x
    opam
    openjdk8
    pandoc
    parallel
    pkgconfig
    python
    python3
    time
    wget
    zip
    z3
  ];
  shellHook = ''
    export PATH=$PWD/node_modules/.bin/:$PWD/bin:$PATH
    export KLAB_EVMS_PATH="''${KLAB_EVMS_PATH:-''${PWD}/evm-semantics}"
    export NODE_OPTIONS="--max-old-space-size=4096"
  '';
}
