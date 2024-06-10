{ mkSbtDerivation, nix-filter, ripgrep, circt, callPackage, riscv-programs, lib }:
{ hash }:
let
  mkDerivation = attrs: mkSbtDerivation ({
    pname = "b4smt";
    version = "0.1.0";
    src = nix-filter {
      root = ../.;
      include = [
        "b4smt"
        "pext"
        "chisel-formal"
        "project"
        "build.sbt"
        ".scalafmt.conf"
      ];
    };
    nativeBuildInputs = [ circt ripgrep ];
    depsSha256 = hash;
    buildPhase = ''
      echo no buildPhase set. Failing.
      false
    '';

    depsWarmupCommand = ''
      sbt "compile;scalafmtCheck" || true
    '';

    fixupPhase = "";
    CHISEL_FIRTOOL_PATH = "${circt}/bin";
  } // attrs);
in
lib.fix (b4smt: mkDerivation {
  buildPhase = ''
    sbt "b4smt/runMain b4smt.B4SMTCore"
    cat B4SMTCore.sv | rg -U '(?s)module B4SMTCore\(.*endmodule' > B4SMTCore.wrapper.v
    sed -i 's/module B4SMTCore(/module B4SMTCoreUnused(/g' B4SMTCore.sv
  '';

  installPhase = ''
    mkdir $out
    cp B4SMTCore.* $out
  '';

  passthru = {
    inherit riscv-programs mkDerivation;
    sbtTest = callPackage ./b4smt_sbt_test.nix {
      inherit b4smt;
    };
  };
})

