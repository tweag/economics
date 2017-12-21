let
  # extend the nixpkgs package set with our own
  myOverlay = self: super: {
    # re-define python3 with our override
    python3 = super.python3.override {
      # extend python package set with our own
      packageOverrides = self: super: {
        # some additional dependencies
        patsy = super.callPackage ./patsy.nix {};
        pymc3 = super.callPackage ./pymc3.nix {};
        jellyfish = super.callPackage ./jellyfish.nix {};
        us = super.callPackage ./us.nix {};

        # inject those dependencies into the jupyter notebook
        notebook = super.notebook.overrideDerivation (oldAttrs: {
          propagatedBuildInputs = with super; oldAttrs.propagatedBuildInputs ++ [
            Theano
            basemap
            matplotlib
            numpy
            seaborn
            pandas
            self.pymc3
            self.us
          ];
        });
      };
    };

    # and finally expose the notebook to the top-level
    notebook = self.python3.pkgs.notebook;
  };
in
# tie everything together with a pinned nixpkgs and remove all impurities
import ./nixpkgs.nix {
  config = {};
  overlays = [ myOverlay ];
}
