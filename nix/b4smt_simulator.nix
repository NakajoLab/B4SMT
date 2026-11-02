{ b4smt, mill, jdk, circt, verilator, stdenv, configFile ? ../simulator/config.json }:
b4smt.mkDerivation {
  pname = "b4smt-simulator";
  nativeBuildInputs = [ mill jdk circt verilator stdenv.cc ];
  buildPhase = ''
    runHook preBuild
    mill --no-daemon --offline b4smt.test.runMain b4smt.simulator.StandaloneConfigTest
    mill --no-daemon --offline b4smt.test.runMain b4smt.simulator.StandaloneSimulator \
      --config ${configFile} --target-dir rtl
    cd rtl
    verilator --cc --exe --build --assert -Wno-fatal \
      --top-module StandaloneSimulator -j "$NIX_BUILD_CORES" \
      -f filelist.f ${../simulator/main.cpp} -o b4smt-sim
    cd ..
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    install -Dm755 rtl/obj_dir/b4smt-sim "$out/bin/b4smt-sim"
    install -Dm644 ${configFile} "$out/share/b4smt-simulator/config.json"
    runHook postInstall
  '';
  meta.mainProgram = "b4smt-sim";
}
