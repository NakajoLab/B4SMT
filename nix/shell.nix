{ mkShell
, circt
, rustfilt
, pkgsCross
, sbt
, jdk
, verilog
, verilator
, zlib
, yosys
, graphviz
, xdot
, espresso
, z3
, symbiyosys
, yices
}:

mkShell {
  name = "b4smt-dev";
  packages = [
    circt
    rustfilt
    pkgsCross.riscv64.stdenv.cc
    sbt
    jdk
    verilog
    verilator
    zlib
    yosys
    graphviz
    xdot
    espresso
    z3
    symbiyosys
    yices
  ];
  CHISEL_FIRTOOL_PATH = "${circt}/bin";
}
