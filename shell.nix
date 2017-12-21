let
  # use our custom version of nixpkgs + overrides
  pkgs = import ./nix;
in
pkgs.stdenv.mkDerivation {
  name = "xxx";
  buildInputs = [
    pkgs.notebook
  ];
}
