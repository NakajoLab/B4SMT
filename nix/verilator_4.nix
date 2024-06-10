{ verilator, fetchFromGitHub }:
verilator.overrideAttrs (old: rec {
  version = "4.228";
  src = fetchFromGitHub {
    owner = "verilator";
    repo = "verilator";
    rev = "v${version}";
    sha256 = "sha256-ToYad8cvBF3Mio5fuT4Ce4zXbWxFxd6smqB1TxvlHao=";
  };
  patches = [ ];
  doCheck = false;
})
