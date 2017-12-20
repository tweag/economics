let
  src = builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs-channels/archive/1bc288591ea4fe3159b7630dcd2b57733d80a2ff.tar.gz;
  };
in
  import src { config = {}; overlays = []; }
