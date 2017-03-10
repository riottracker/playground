{ mkDerivation, base, enumset, stdenv, stm, vty, alsaLib
}:
mkDerivation {
  pname = "sequencer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base vty stm alsaLib
  ];
  homepage = "https://github.com/githubuser/seq#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
