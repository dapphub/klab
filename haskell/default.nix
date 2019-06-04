{ mkDerivation, aeson, base, containers, lens, mtl
, optparse-applicative, parsec, safe, stdenv, utf8-string
}:
mkDerivation {
  pname = "k-gas-analyser";
  version = "0.1.3";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers lens mtl optparse-applicative parsec safe
    utf8-string
  ];
  homepage = "https://github.com/dapphub/klab";
  description = "a gas analysis tool for use with K specifications written in the act language";
  license = stdenv.lib.licenses.agpl3;
}
