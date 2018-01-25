{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

f = { mkDerivation, base, inline-r, integration, R, random, stdenv
    , template-haskell, temporary }:
mkDerivation {
  pname = "mrp";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    inline-r
    integration
    random
    template-haskell
    temporary ];
  executableSystemDepends = [
    R
    pkgs.rPackages.cmaes
    pkgs.rPackages.ggplot2
    pkgs.rPackages.numDeriv
    pkgs.rPackages.optimx
    pkgs.rPackages.reshape2
    # rJava fails with ld: framework not found JavaVM and rCMA won't
    # work without it albeit it still installs so maybe a dependency
    # is missing
    # pkgs.rPackages.rCMA
    # pkgs.rPackages.rJava
    pkgs.rPackages.rstan ];
  license = stdenv.lib.licenses.bsd3;
};

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

