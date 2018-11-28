{ nixpkgs ? (import <nixpkgs> {}).fetchFromGitHub {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "8c2c14ac392e5b96a1b3d12e16ba0439689024c7";
  sha256 = "0x3b0ml7gxc9y28y4l64mx6w5582ncks0rca00ikn1pqffffvbxi";
}, compiler ? "ghc822" }:
let
  pkgs = import nixpkgs { config = {}; };
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          cabal-install hoogle
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "k-gas-analyser";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
