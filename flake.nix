{
  description = "riscv test flake";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    #    nixpkgs-stable.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    riscv-test-src = {
      url = "https://github.com/riscv-software-src/riscv-tests";
      type = "git";
      submodules = true;
      flake = false;
    };
    nix-sbt = {
      url = "github:zaninime/sbt-derivation";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, riscv-test-src, nix-sbt, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        nf = import nix-filter;
        B4ProcessorDerivation = attrs: nix-sbt.mkSbtDerivation.x86_64-linux ({
          pname = "B4Processor";
          version = "0.1.0";
          src = nf {
            root = ./.;
            include = [
              "src"
              "project"
              "build.sbt"
            ];
          };
          buildInputs = with pkgs; [ circt ripgrep ];
          depsSha256 = "sha256-oG92yXLLHHuVYQvbUTDiX8qeLE45mrQjwdMZ8TEfFA0=";
          buildPhase = ''
            sbt "runMain b4processor.B4Processor"
            cat B4Processor.sv | rg -U '(?s)module B4Processor\(.*endmodule' > B4Processor.wrapper.v
            sed -i 's/module B4Processor(/module B4ProcessorUnused(/g' B4Processor.sv
          '';

          # install your software in $out, depending on your needs
          installPhase = ''
            mkdir $out
            cp B4Processor.* $out
          '';

          fixupPhase = "true";
        } // attrs);
        sbtTest = testCommand: B4ProcessorDerivation {
          pname = "B4Processor-tests";
          buildInputs = with pkgs; [
            verilog
            verilator
            stdenv.cc
            zlib
            circt
          ];
          buildPhase = ''
            ln -s ${self.packages.${system}.default} programs
            ${testCommand}
          '';
          installPhase = ''
            mkdir $out
            [ -d test_run_dir ] && cp -r test_run_dir $out || true
          '';
        };
      in
      {
        packages = rec {
          riscv-tests = import ./riscv-tests-files/riscv-tests.nix { inherit pkgs riscv-test-src; };
          riscv-sample-programs = import ./riscv-sample-programs/sample-programs.nix { inherit pkgs; };
          processor = B4ProcessorDerivation { };
          default = pkgs.linkFarm "processor test programs" [
            { name = "riscv-tests"; path = riscv-tests; }
            { name = "riscv-sample-programs"; path = riscv-sample-programs; }
          ];
          slowChecks = sbtTest ''sbt "testOnly * -- -n org.scalatest.tags.Slow"'';
        };
        checks =
          {
            quick = sbtTest ''sbt "testOnly * -- -l org.scalatest.tags.Slow"'';
          };
        formatter = pkgs.nixpkgs-fmt;
        devShells.default = pkgs.mkShell {
          name = "processor-shell";
          buildInputs = with pkgs;[
            circt
            rustfilt
            pkgsCross.riscv64-embedded.buildPackages.gcc
            sbt
            jdk
            verilog
            verilator
            zlib
          ];
        };
      });
}
