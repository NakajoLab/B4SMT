{ llvmPackages, riscvCompiler, autoreconfHook, stdenvNoCC, riscv-test-src }:
stdenvNoCC.mkDerivation {
  name = "riscv-tests";
  src = riscv-test-src;
  configureFlags = [ "target_alias=riscv64-none-elf" ];
  enableParallelBuilding = true;
  nativeBuildInputs = [
    llvmPackages.bintools
    riscvCompiler
    autoreconfHook
  ];
  postInstall = ''
    find -L $out -name Makefile | xargs rm
  '';
  fixupPhase = "true";
}
