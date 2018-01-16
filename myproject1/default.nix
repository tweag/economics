{ mkDerivation, base, stdenv, hmatrix }:
mkDerivation {
  pname = "project1";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hmatrix ];
  license = stdenv.lib.licenses.bsd3;
}
