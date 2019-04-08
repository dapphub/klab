with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "klab-env";
  buildInputs = [
    autoconf
    flex
    gcc
    getopt
    git
    gmp
    gnumake
    maven
    mpfr
    ncurses
    nodejs
    opam
    openjdk8
    pandoc
    parallel
    pkgconfig
    python
    python3
    z3
  ];
  shellHook = ''
    export PATH=$PWD/node_modules/.bin/:$PWD/bin:$PATH
    export KLAB_EVMS_PATH="''${KLAB_EVMS_PATH:-''${PWD}/evm-semantics}"
  '';
}
