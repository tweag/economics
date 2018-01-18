{ compiler ? "ghc822" }:

let
  overlay = self: pkgs: {
    haskellPackages = pkgs.haskell.packages.${compiler}.override {
      overrides = hsSelf: hsPkgs: {
        project1 = hsSelf.callPackage ./default.nix { };
        hmatrix = hsSelf.callPackage ./hmatrix.nix { };
      };
    };
  };

  pkgs = import <nixpkgs> {
    config = {};
    overlays = [ overlay ];
  };
in
  {
    inherit (pkgs.haskellPackages) project1;
  }
