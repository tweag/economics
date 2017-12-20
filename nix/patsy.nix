{ buildPythonPackage, fetchPypi, nose, six, numpy }:
buildPythonPackage rec {
  pname = "patsy";
  name = "patsy-${version}";
  version = "0.4.1";
  format = "wheel";

  src = fetchPypi {
    inherit pname version format;
    sha256 = "0b7jh7k4q15mc8v84h8as031wb8ixay5g7dzwczhqsswvxvjy433";
  };

  buildInputs = [ nose ];
  propagatedBuildInputs = [six numpy];
}
