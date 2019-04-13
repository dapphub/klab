with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "klab-env";
  buildInputs = [
    autoconf
    flex
    utillinux
    gcc
    getopt
    git
    gmp
    gnumake
    jq
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
    zip
    z3
    getopt
  ];
  shellHook = ''
    export PATH=$PWD/node_modules/.bin/:$PWD/bin:$PATH
    export KLAB_EVMS_PATH="''${KLAB_EVMS_PATH:-''${PWD}/evm-semantics}"
  '';
}
