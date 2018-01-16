let
  overlay = self: pkgs: {
    haskellPackages = pkgs.haskellPackages.override {
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
