{ stdenv, nix-filter, ripgrep, circt, callPackage, riscv-programs, lib, mill, jdk }:
{ hash }:
let
  src = nix-filter {
    root = ../.;
    include = [ "b4smt" "pext" "chisel-formal" "build.mill" ".scalafmt.conf" ];
  };
  dependencies = stdenv.mkDerivation {
    pname = "b4smt-mill-dependencies";
    version = "0.1.0";
    inherit src;
    nativeBuildInputs = [ mill jdk ];
    outputHashMode = "recursive";
    outputHashAlgo = "sha256";
    outputHash = hash;
    buildPhase = ''
      export HOME="$TMPDIR/home" COURSIER_CACHE="$TMPDIR/coursier"
      mkdir -p "$HOME" "$COURSIER_CACHE"
      mill --no-daemon mill.javalib.OfflineSupport/prepareOffline '__:OfflineSupportModule.prepareOffline'
      mill --no-daemon b4smt.test.compile
      mill --no-daemon mill.javalib.JvmWorkerModule/scalalibClasspath
      # Scalafmt resolves its configured version dynamically, outside prepareOffline.
      mill --no-daemon mill.scalalib.scalafmt/reformatAll
    '';
    installPhase = ''
      mkdir -p "$out"
      cp -R "$COURSIER_CACHE"/. "$out"/
      find "$out" -type f \( -name '*.lock' -o -name '*.checked' \) -delete
    '';
  };
  mkDerivation = attrs: stdenv.mkDerivation ({
    pname = "b4smt";
    version = "0.1.0";
    inherit src;
    nativeBuildInputs = [ mill jdk circt ripgrep ];
    preBuild = ''
      export HOME="$TMPDIR/home" COURSIER_CACHE="$TMPDIR/coursier"
      mkdir -p "$HOME" "$COURSIER_CACHE"
      cp -R ${dependencies}/. "$COURSIER_CACHE"/
      chmod -R u+w "$COURSIER_CACHE"
    '';
    CHISEL_FIRTOOL_PATH = "${circt}/bin";
    dontFixup = true;
  } // attrs);
in
lib.fix (b4smt: mkDerivation {
  buildPhase = ''
    runHook preBuild
    mill --no-daemon --offline b4smt.runMain b4smt.B4SMTCore
    cat B4SMTCore.sv | rg -U '(?s)module B4SMTCore\(.*endmodule' > B4SMTCore.wrapper.v
    sed -i 's/module B4SMTCore(/module B4SMTCoreUnused(/g' B4SMTCore.sv
    runHook postBuild
  '';
  installPhase = ''
    mkdir $out
    while IFS= read -r file; do
      cp --parents "$file" "$out"
    done < filelist.f
    cp filelist.f B4SMTCore.wrapper.v $out/
  '';
  passthru = {
    inherit dependencies riscv-programs mkDerivation;
    millTest = callPackage ./b4smt_mill_test.nix { inherit b4smt; };
  };
})
