let
  pkgs = import ./nix/nixpkgs.nix;
  pythonPackages = p: with p; let
    patsy = callPackage ./nix/patsy.nix {};
    pymc3 = callPackage ./nix/pymc3.nix { inherit Theano patsy; };
    jellyfish = callPackage ./nix/jellyfish.nix {};
    us = callPackage ./nix/us.nix { inherit jellyfish; };
  in [
    ipykernel
    ipywidgets
    jupyter_console
    nbconvert
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
