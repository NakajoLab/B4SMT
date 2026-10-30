{ riscvCompiler, stdenvNoCC, nix-filter }:

stdenvNoCC.mkDerivation {
  name = "riscv-sample-programs";
  src = nix-filter {
    root = ./.;
    exclude = [
      (nix-filter.matchExt "o")
      (nix-filter.matchExt "binary")
    ];
  };
  nativeBuildInputs = [
    riscvCompiler
  ];
  enableParallelBuilding = true;
  installPhase = "
    mkdir $out
    cp -fv **/*.{hex,bin,dump} $out
    cp -fv ${./bench.hex.generated} $out/bench.hex
  ";
}
