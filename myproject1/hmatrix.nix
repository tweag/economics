{ mkDerivation, array, base, binary, openblas, bytestring, deepseq
, liblapack, random, semigroups, split, stdenv, storable-complex
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
  buildDepends = stdenv.lib.optionals stdenv.isDarwin darwin.apple_sdk.frameworks.Accelerate;
  configureFlags = [
    "-fdisable-default-paths"
    "-fopenblas"
  ];
  libraryHaskellDepends = [
    array base binary bytestring deepseq random semigroups split
    storable-complex vector
  ];
  librarySystemDepends = [ openblas liblapack ];
  # preConfigure = "sed -i hmatrix.cabal -e '/\\/usr\\//D'";
  homepage = "https://github.com/albertoruiz/hmatrix";
  description = "Numeric Linear Algebra";
  license = stdenv.lib.licenses.bsd3;
}
