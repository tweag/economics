{ buildPythonPackage, fetchPypi, pytest, unicodecsv }:
buildPythonPackage rec {
  pname = "jellyfish";
  version = "0.5.6";
  name = "${pname}-${version}";

  buildInputs = [ pytest unicodecsv ];

  src = fetchPypi {
    inherit pname version;
    sha256 = "1j9rplb16ba2prjj6mip46z0w9pnhnqpwgiwi0x93vnas14rlyl8";
  };
}
