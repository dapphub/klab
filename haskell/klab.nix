{ mkDerivation, aeson, base, optparse-applicative, parsec, safe
, stdenv, utf8-string
}:
mkDerivation {
  pname = "k-gas-analyser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base optparse-applicative parsec safe utf8-string
  ];
  homepage = "https://github.com/dapphub/k-gas-analyser";
  description = "analyser for KEVM symbolic gas expressions";
  license = stdenv.lib.licenses.agpl3;
}
