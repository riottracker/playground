{ mkDerivation, base, stdenv, alsaLib }:
mkDerivation {
  pname = "trig";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base alsaLib
  ];
  license = stdenv.lib.licenses.bsd3;
}

