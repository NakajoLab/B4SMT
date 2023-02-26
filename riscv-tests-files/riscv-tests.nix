{ pkgs, riscv-test-src }:
pkgs.stdenv.mkDerivation {
  name = "riscv-tests";
  src = riscv-test-src;
  patches = [ ./riscv-test.patch ];
  configureFlags = [ "target_alias=riscv64-none-elf" ];
  enableParallelBuilding = true;
  buildInputs = with pkgs; [ llvmPackages.bintools pkgs.pkgsCross.riscv64-embedded.buildPackages.gcc autoreconfHook circt ];
  postInstall = ''
    find -L $out -name Makefile | xargs rm
  '';
  fixupPhase = "true";
}
