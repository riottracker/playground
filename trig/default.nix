let pkgs = import <nixpkgs> {};
in pkgs.haskellPackages.callPackage ./trig.nix { }


