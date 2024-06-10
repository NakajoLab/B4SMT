{ b4smt, verilog, verilator_4, stdenv, zlib, circt, yosys, yices, espresso, z3, symbiyosys }:
name: testCommand:
b4smt.mkDerivation {
  name = "${name}-b4smt-test";
  nativeBuildInputs = [
    verilog
    verilator_4
    stdenv.cc
    zlib
    circt
    yosys
    yices
    espresso
    z3
    symbiyosys
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
