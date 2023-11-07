{ pkgs }: pkgs.stdenv.mkDerivation {
  name = "riscv-sample-programs";
  src = ./.;
  nativeBuildInputs = with pkgs;[
    pkgsCross.riscv64-embedded.buildPackages.gcc
    circt
  ];
  #  enableParallelBuilding = true;
  installPhase = "
    mkdir $out
    cp -fv **/*.{hex,bin,dump} $out
    cp -fv ${./bench.hex.generated} $out/bench.hex
  ";
  fixupPhase = "true";
}
