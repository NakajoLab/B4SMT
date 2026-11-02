{ mkShell
, circt
, pkgsCross
, mill
, which
, jdk
, iverilog
, verilator
, zlib
, yosys
, graphviz
, xdot
, espresso
, z3
, sby
, yices
}:

mkShell {
  name = "b4smt-dev";
  packages = [
    circt
    pkgsCross.riscv64.stdenv.cc
    mill
    which
    jdk
    iverilog
    verilator
    zlib
    yosys
    # graphviz
    # xdot
    espresso
    z3
    sby
    yices
  ];
  CHISEL_FIRTOOL_PATH = "${circt}/bin";
}
