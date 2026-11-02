{ b4smt, mill, jdk, which, python3, iverilog, verilator, stdenv, zlib, circt, yosys, yices, espresso, z3, sby }:
name: testCommand:
b4smt.mkDerivation {
  name = "${name}-b4smt-test";
  nativeBuildInputs = [
    mill
    which
    python3
    jdk
    iverilog
    verilator
    stdenv.cc
    zlib
    circt
    yosys
    yices
    espresso
    z3
    sby
  ];
  buildPhase = ''
    runHook preBuild
    ln -s ${b4smt.riscv-programs} programs
    ${testCommand}
    runHook postBuild
  '';
  installPhase = ''
    mkdir $out
    [ -d test_run_dir ] && cp -r test_run_dir $out || true
  '';
}
