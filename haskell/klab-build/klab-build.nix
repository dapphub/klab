{ mkDerivation, base, stdenv, optparse-applicative, pretty-show }:
mkDerivation {
  pname = "klab-build";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base optparse-applicative pretty-show ];
  homepage = "https://dapphub.com";
  description = "Smart contract specification format";
  license = stdenv.lib.licenses.agpl3;
}
