{
  description = "riscv test flake";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
  # Older RISC-V tests require legacy CSR aliases removed by newer binutils.
  inputs.nixpkgs-riscv.url = "github:nixos/nixpkgs/cd18e2ae9ab8e2a0a8d715b60c91b54c0ac35ff9";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.sbt-derivation = {
    url = "github:zaninime/sbt-derivation";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };
  inputs.riscv-test-src = {
    url = "https://github.com/pineapplehunter/riscv-tests";
    type = "git";
    submodules = true;
    flake = false;
  };

  outputs = { self, nixpkgs, ... }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = final: prev: {
          verilator_4 = final.callPackage ./nix/verilator_4.nix { };
          circt = final.callPackage ./nix/firtool.nix { };
          b4smtGen = final.callPackage ./nix/b4smtgen.nix {
            riscv-programs = self.packages.${system}.default;
          };
          b4smt = final.b4smtGen { hash = "sha256-tBD2gT5yOAssshRrh2Ky+XL7kPC6MxS2X4HhKqk+y30="; };
          sbt = prev.sbt.override { jre = final.jre_headless; };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.nix-filter.overlays.default
            inputs.sbt-derivation.overlays.default
            overlays
          ];
          config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
            "espresso"
          ];
        };
        inherit (nixpkgs) lib;
      in
      {
        packages = rec {
          riscv-tests = pkgs.callPackage ./riscv-tests-files {
            inherit (inputs) riscv-test-src;
            riscvCompiler = (import inputs.nixpkgs-riscv { inherit system; }).pkgsCross.riscv64-embedded.stdenv.cc;
          };
          riscv-sample-programs = pkgs.callPackage ./riscv-sample-programs/sample-programs.nix {
            riscvCompiler = (import inputs.nixpkgs-riscv { inherit system; }).pkgsCross.riscv64-embedded.stdenv.cc;
          };
          processor = pkgs.b4smt;
          default = pkgs.linkFarm "processor test programs" [
            { name = "riscv-tests"; path = riscv-tests; }
            { name = "riscv-sample-programs"; path = riscv-sample-programs; }
          ];
          slowChecks = pkgs.b4smt.sbtTest "slow" ''sbt "testOnly * -- -n org.scalatest.tags.Slow"'';
          verilator = pkgs.verilator_4;
          format = pkgs.b4smt.sbtTest "format" ''sbt fmtCheck'';
        };

        checks =
          {
            quick = pkgs.b4smt.sbtTest "quick" ''sbt "testOnly * -- -l org.scalatest.tags.Slow"'';
            # programs = sbtTest ''sbt "testOnly *ProgramTest*"'';
          };

        formatter = pkgs.nixpkgs-fmt;

        devShells.default = pkgs.callPackage ./nix/shell.nix {
          verilator = pkgs.verilator_4;
        };

        apps.update-hash = {
          type = "app";
          program = let
            script = pkgs.writeShellScript "update-hash" ''
              echo updating-hash
              set -xe
              export hash=$(nix eval ".#processor.dependencies.outputHash" --json | ${lib.getExe pkgs.jq} -r)
              sed -i "s|$hash|${lib.fakeHash}|" flake.nix
              echo this is a temporary file for updating the hash > update-tmp
              nix build ".#processor.dependencies" --no-link -L |& tee -a update-tmp
              export new_hash=$(grep "got:" update-tmp | tail -n1 | awk '{print $2}')
              sed -i "s|${lib.fakeHash}|$new_hash|" flake.nix
              rm update-tmp
              echo "changed hash from:$hash to:$new_hash"
            '';
          in "${script}";
        };
      });
}
