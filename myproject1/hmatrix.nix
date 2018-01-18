{ mkDerivation, array, base, binary, openblas, bytestring, deepseq
, random, semigroups, split, stdenv, storable-complex
, vector
, fetchFromGitHub
, darwin
}:
let
  src = fetchFromGitHub {
    owner = "albertoruiz";
    repo = "hmatrix";
    rev = "0d4018b09f1d38bbc05fdc0d8197d0f8a6ce5571";
    sha256 = "0l6pvgkrykjkdi3q71qh7b2fk5alnf9qr9fi80dwj9xyaqpiwn3l";
  };
in
mkDerivation {
  pname = "hmatrix";
  version = "0.18.1.1";
  src = "${src}/packages/base";
  buildDepends = [ (stdenv.lib.optionals stdenv.isDarwin darwin.apple_sdk.frameworks.Accelerate) ];
  configureFlags = [
    "-fdisable-default-paths"
    "-fopenblas"
  ];
  libraryHaskellDepends = [
    array base binary bytestring deepseq random semigroups split
    storable-complex vector
  ];
  librarySystemDepends = [ openblas ];
  # preConfigure = "sed -i hmatrix.cabal -e '/\\/usr\\//D'";
  homepage = "https://github.com/albertoruiz/hmatrix";
  description = "Numeric Linear Algebra";
  license = stdenv.lib.licenses.bsd3;
}

# I have many questions:
#
# 1. I would like to use the blas that comes with OSX but if I replace
# openblas by blas, this derivation(?) picks up the Haskell package
# called blas not the nix package called blas
#
# bash-3.2$ nix-env -qaPf /Users/dom/nixpkgs |
# grep -i blas
#
# blas                                                   blas-3.7.1
#
# bash-3.2$ nix-env -f /Users/dom/nixpkgs -qaP -A haskellPackages |
#           grep -i blas
#
# haskellPackages.blas                                         blas-0.7.6

# 2. If I uncomment the line which says
# 
#   `preConfigure = "sed -i hmatrix.cabal -e '/\\/usr\\//D'";`
# 
# then I get a cabal error. How should I change the version of cabal
# that is being used?
# 
# Answer: See release1.nix changes
