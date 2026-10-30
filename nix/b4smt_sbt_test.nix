{ b4smt, iverilog, verilator_4, stdenv, zlib, circt, yosys, yices, espresso, z3, sby }:
name: testCommand:
b4smt.mkDerivation {
  name = "${name}-b4smt-test";
  nativeBuildInputs = [
    iverilog
    verilator_4
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
    ln -s ${b4smt.riscv-programs} programs
    ${testCommand}
  '';
  installPhase = ''
    mkdir $out
    [ -d test_run_dir ] && cp -r test_run_dir $out || true
  '';
}
