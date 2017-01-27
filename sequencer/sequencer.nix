{ mkDerivation, base, enumset, PortMidi, stdenv, stm, transformers
, vty
}:
mkDerivation {
  pname = "sequencer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base enumset PortMidi stm transformers vty
  ];
  homepage = "https://github.com/githubuser/seq#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}
