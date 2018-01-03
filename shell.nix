let
  # use our custom version of nixpkgs + overrides
  pkgs = import ./nix;
in
pkgs.stdenv.mkDerivation {
  name = "xxx";

  buildInputs = with pkgs.rPackages; [
    pkgs.scipy
    pkgs.R
    ggplot2
    dplyr
    maps
    Cairo
    knitr
    arm
    foreign
    maps
    plyr
    mapproj
  ];
}
