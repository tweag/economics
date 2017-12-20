{ buildPythonPackage, fetchPypi, jellyfish }:
buildPythonPackage rec {
  pname = "us";
  version = "1.0.0";
  name = "${pname}-${version}";

  propagatedBuildInputs = [ jellyfish ];

  src = fetchPypi {
    inherit pname version;
    sha256 = "1niglalkp7pinibzbxjdz9mxx9qmwkrh8884dag3kr72cfkrpp09";
  };
}
