{ mkDerivation, array, base, binary, openblas, bytestring, deepseq
, liblapack, random, semigroups, split, stdenv, storable-complex
, vector
}:
mkDerivation {
  pname = "hmatrix";
  version = "0.18.1.1";
  src = /Users/dom/hmatrix/packages/base;
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
