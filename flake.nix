{
  description = "riscv test flake";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
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
          b4smtGen = final.callPackage ./nix/b4smtgen.nix {
            riscv-programs = self.packages.${system}.default;
          };
          b4smt = final.b4smtGen { hash = "sha256-zQBZlFNFLPZL1O31+p2ICGV1AKahRwYtXZhqCsrkrCo="; };
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
          riscv-tests = pkgs.callPackage ./riscv-tests-files { inherit (inputs) riscv-test-src; };
          riscv-sample-programs = pkgs.callPackage ./riscv-sample-programs/sample-programs.nix { };
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
