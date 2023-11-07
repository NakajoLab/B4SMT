{ llvmPackages_16, pkgsCross, autoreconfHook, circt, stdenv, riscv-test-src }: stdenv.mkDerivation {
  name = "riscv-tests";
  src = riscv-test-src;
  patches = [ ./riscv-test.patch ];
  configureFlags = [ "target_alias=riscv64-none-elf" ];
  enableParallelBuilding = true;
  buildInputs = [
    llvmPackages_16.bintools
    pkgsCross.riscv64-embedded.buildPackages.gcc
    autoreconfHook
    circt
  ];
  postInstall = ''
    find -L $out -name Makefile | xargs rm
  '';
  fixupPhase = "true";
}
