{ buildPythonPackage, fetchPypi, Theano }:

buildPythonPackage rec {
  pname = "pymc3";
  version = "3.2";
  name = "${pname}-${version}";

  buildInputs = [ Theano ];

  src = fetchPypi {
    inherit pname version;
    sha256 = "0hpzhkpv7sbwkcva7x914yvzcf1d1a952ynbcx6mvlgv5lqghc39";
  };
}
