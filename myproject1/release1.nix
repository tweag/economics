let
config = {
  packageOverrides = pkgs: rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        project1 =
          haskellPackagesNew.callPackage ./default.nix { };
        hmatrix =
          if pkgs.stdenv.isDarwin
          then pkgs.haskell.lib.addBuildDepends (pkgs.haskell.lib.enableCabalFlag (pkgs.haskell.lib.enableCabalFlag (haskellPackagesNew.callPackage ./hmatrix.nix { }) "openblas") "disable-default-paths") pkgs.darwin.apple_sdk.frameworks.Accelerate
          else pkgs.haskell.lib.enableCabalFlag (pkgs.haskell.lib.enableCabalFlag (haskellPackagesNew.callPackage ./hmatrix.nix { }) "openblas") "disable-default-paths";
      };
    };
  };
};

pkgs = import <nixpkgs> { inherit config; };

in
{ project1 = pkgs.haskellPackages.project1;
}

# hmatrix = if pkgs.stdenv.isDarwin                                             
#     then addBuildDepend super.hmatrix pkgs.darwin.apple_sdk.frameworks.Accelerate
#     else super.hmatrix; 
