{ buildPythonPackage, fetchPypi
, Theano
, h5py
, joblib
, nose
, pandas
, patsy
, tqdm
}:

buildPythonPackage rec {
  pname = "pymc3";
  version = "3.2";
  name = "${pname}-${version}";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0hpzhkpv7sbwkcva7x914yvzcf1d1a952ynbcx6mvlgv5lqghc39";
  };

  doCheck = false;

  buildInputs = [ nose ];

  propagatedBuildInputs = [
    Theano
    h5py
    joblib
    pandas
    patsy
    tqdm
  ];
}
