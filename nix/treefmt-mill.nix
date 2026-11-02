{ config, lib, pkgs, ... }:
{
  options.programs.mill.enable = lib.mkEnableOption "Scala formatting through Mill";

  config = lib.mkIf config.programs.mill.enable {
    settings.formatter.mill = {
      command = lib.getExe (pkgs.writeShellApplication {
        name = "mill-fmt";
        runtimeInputs = [ pkgs.mill pkgs.jdk ];
        text = ''
          export JAVA_HOME=${pkgs.jdk}
          # Mill formats the whole source tree, not individual file arguments.
          mill --no-daemon mill.scalalib.scalafmt/reformatAll
        '';
      });
      includes = [ "*.scala" "*.mill" ".scalafmt.conf" ];
    };
  };
}
