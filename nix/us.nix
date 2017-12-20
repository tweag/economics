{ buildPythonPackage, fetchPypi }:
buildPythonPackage rec {
  pname = "us";
  version = "1.0.0";
  name = "${pname}-${version}";

  buildInputs = [ ];

  src = fetchPypi {
    inherit pname version;
    sha256 = "1niglalkp7pinibzbxjdz9mxx9qmwkrh8884dag3kr72cfkrpp09";
  };
}
