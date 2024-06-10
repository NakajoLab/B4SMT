{ llvmPackages_16, pkgsCross, autoreconfHook, stdenvNoCC, riscv-test-src }:
stdenvNoCC.mkDerivation {
  name = "riscv-tests";
  src = riscv-test-src;
  configureFlags = [ "target_alias=riscv64-none-elf" ];
  enableParallelBuilding = true;
  nativeBuildInputs = [
    llvmPackages_16.bintools
    pkgsCross.riscv64-embedded.stdenv.cc
    autoreconfHook
  ];
  postInstall = ''
    find -L $out -name Makefile | xargs rm
  '';
  fixupPhase = "true";
}
