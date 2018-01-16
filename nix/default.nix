let
  # extend the nixpkgs package set with our own
  myOverlay = self: pkgs: {
    # re-define python3 with our override
    python3 = pkgs.python3.override {
      # extend python package set with our own
      packageOverrides = self: super: {
        # some additional dependencies
        patsy = super.callPackage ./patsy.nix {};
        pymc3 = super.callPackage ./pymc3.nix {};
        jellyfish = super.callPackage ./jellyfish.nix {};
        us = super.callPackage ./us.nix {};
        Theano = super.Theano.overrideDerivation (oldAttrs: {
          # remove libgpuarray as it doesn't build on macOS
          buildInputs = pkgs.lib.remove super.libgpuarray oldAttrs.buildInputs;
        });

        # inject those dependencies into the jupyter notebook
        notebook = super.notebook.overrideDerivation (oldAttrs: {
          propagatedBuildInputs = with super; oldAttrs.propagatedBuildInputs ++ [
            basemap
            matplotlib
            numpy
            seaborn
            pandas
            self.pymc3
            self.us
            # self.Theano
          ];
        });
      };
    };

    # Scientific python
    scipy = self.python3.withPackages(p: with p; [
      basemap
      matplotlib
      numpy
      pandas
      pymc3
      seaborn
      us
    ]);

    # and finally expose the notebook to the top-level
    notebook = self.python3.pkgs.notebook;
  };
in
# tie everything together with a pinned nixpkgs and remove all impurities
import ./nixpkgs.nix {
  config = {};
  overlays = [ myOverlay ];
}
