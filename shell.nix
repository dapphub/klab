let
  pkgs = import (builtins.fetchGit rec {
    name = "nixpkgs-19.09-${rev}";
    url = https://github.com/nixos/nixpkgs;
    ref = "nixos-20.03";
    # git ls-remote https://github.com/nixos/nixpkgs-channels nixos-20.03
    rev = "0bb35152be895abfd1fc743b42f1c4e56ae71906";
  }) {};

  act = import (builtins.fetchGit {
    url = "https://github.com/ethereum/act.git";
    rev = "db3f56c4b2d297b1f3db626a83202bd943fed607";
    ref = "kbackend";
  }) {};
in
pkgs.stdenv.mkDerivation {
  passthru = { inherit pkgs; };
  name = "klab-env";
  buildInputs = with pkgs; [
    act
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
    unzip
    wget
    z3
    zstd
  ];
  shellHook = ''
    export PATH=${toString ./node_modules/.bin}:${toString ./bin}:$PATH
    export KLAB_EVMS_PATH="''${KLAB_EVMS_PATH:-${toString ./evm-semantics}}"
    export NODE_OPTIONS="--max-old-space-size=4096"
  '';
}
