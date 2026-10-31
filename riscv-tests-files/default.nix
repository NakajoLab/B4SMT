{ llvmPackages, pkgsCross, autoreconfHook, stdenvNoCC, riscv-test-src }:
stdenvNoCC.mkDerivation {
  name = "riscv-tests";
  src = riscv-test-src;
  patches = [ ./fork-changes.patch ];
  postPatch = ''
    cp -r ${./benchmarks}/. benchmarks/
  '';
  configureFlags = [ "target_alias=riscv64-none-elf" ];
  enableParallelBuilding = true;
  nativeBuildInputs = [
    llvmPackages.bintools
    pkgsCross.riscv64-embedded.stdenv.cc
    autoreconfHook
  ];
  postInstall = ''
    cp isa/*.hex $out/share/riscv-tests/isa/
    cp benchmarks/*.hex $out/share/riscv-tests/benchmarks/
  '';
  fixupPhase = "true";
}
