{ stdenv, fetchurl, autoPatchelfHook, zlib }:
stdenv.mkDerivation rec {
  pname = "firtool-bin";
  version = "1.73.0";
  src = fetchurl {
    url = "https://github.com/llvm/circt/releases/download/firtool-${version}/firrtl-bin-linux-x64.tar.gz";
    hash = "sha256-2NNisCIPrA1inxDNudAp9VYTXUZCu5ClUb1rKyhmY0U=";
  };
  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ zlib stdenv.cc.cc.lib ];
  installPhase = ''
    mkdir -p $out/bin
    cp bin/firtool $out/bin/
  '';
  meta.platforms = [ "x86_64-linux" ];
}
