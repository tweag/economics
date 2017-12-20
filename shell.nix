let
  pkgs = import <nixpkgs> {};
  pythonPackages = p: with p; let
    pymc3 = callPackage ./nix/pymc3.nix { inherit Theano; };
    us = callPackage ./nix/us.nix {};
  in [
    notebook

    # dependencies
    Theano
    matplotlib
    numpy
    pandas
    pymc3
    us
  ];
in
  pkgs.python3.withPackages pythonPackages
