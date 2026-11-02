{
  description = "riscv test flake";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.treefmt-nix = {
    url = "github:numtide/treefmt-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.riscv-test-src = {
    url = "https://github.com/riscv-software-src/riscv-tests";
    type = "git";
    submodules = true;
    flake = false;
  };

  outputs = { self, nixpkgs, ... }@inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = final: prev: {
          b4smtGen = final.callPackage ./nix/b4smtgen.nix {
            riscv-programs = self.packages.${system}.default;
          };
          b4smt = final.b4smtGen { hash = "sha256-utcJYpKsnVZfsNWksD1oQjI9uGy60iMErngk4TZ14s0="; };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.nix-filter.overlays.default
            overlays
          ];
          config.allowUnfreePredicate = pkg: builtins.elem (nixpkgs.lib.getName pkg) [
            "espresso"
          ];
        };
        inherit (nixpkgs) lib;
        treefmt = inputs.treefmt-nix.lib.evalModule pkgs {
          imports = [ ./nix/treefmt-mill.nix ];
          projectRootFile = "flake.nix";
          programs.nixpkgs-fmt.enable = true;
          programs.mill.enable = true;
        };
      in
      {
        packages = rec {
          riscv-tests = pkgs.callPackage ./riscv-tests-files { inherit (inputs) riscv-test-src; };
          riscv-sample-programs = pkgs.callPackage ./riscv-sample-programs/sample-programs.nix { };
          processor = pkgs.b4smt;
          simulator = pkgs.callPackage ./nix/b4smt_simulator.nix { };
          default = pkgs.linkFarm "processor test programs" [
            { name = "riscv-tests"; path = riscv-tests; }
            { name = "riscv-sample-programs"; path = riscv-sample-programs; }
          ];
          slowChecks = pkgs.b4smt.millTest "slow" ''mill --no-daemon --offline b4smt.test.testOnly '*' -- -n org.scalatest.tags.Slow'';
          verilator = pkgs.verilator;
          format = pkgs.b4smt.millTest "format" ''mill --no-daemon --offline mill.scalalib.scalafmt/checkFormatAll'';
        };

        checks =
          {
            quick = pkgs.b4smt.millTest "quick" ''mill --no-daemon --offline b4smt.test.testOnly '*' -- -l org.scalatest.tags.Slow'';
            simulator = pkgs.runCommand "b4smt-simulator-smoke" { } ''
              sim=${self.packages.${system}.simulator}/bin/b4smt-sim
              samples=${self.packages.${system}.riscv-sample-programs}
              "$sim" --program "$samples/branch.hex" --max-cycles 200 --expect-register 13 20
              "$sim" --program "$samples/fibonacci.hex" --max-cycles 400 --expect-register 6 55
              if "$sim" --program "$samples/branch.hex" --max-cycles 1 --expect-register 13 20; then
                echo "expected timeout did not fail" >&2
                exit 1
              fi
              printf 'wait 0 13 20 200\nexpect 0 13 20\n' > valid.plan
              "$sim" --program "$samples/branch.hex" --plan valid.plan
              reject_plan() {
                if "$sim" --program "$samples/branch.hex" --plan "$1" > failure.log 2>&1; then
                  echo "invalid plan unexpectedly passed: $1" >&2
                  exit 1
                fi
                grep -F "$2" failure.log
              }
              mkdir unreadable.plan
              reject_plan unreadable.plan 'cannot read plan'
              touch empty.plan
              reject_plan empty.plan 'plan contains no actions'
              printf 'uart 500 0 4294967295\n' > invalid-uart.plan
              reject_plan invalid-uart.plan 'UART expectation must be a byte'
              printf 'search 500 0 1 1 256\n' > invalid-search.plan
              reject_plan invalid-search.plan 'UART search value must be a byte'
              touch "$out"
            '';
          };

        formatter = treefmt.config.build.wrapper;

        devShells.default = pkgs.callPackage ./nix/shell.nix { };

        apps.update-hash = {
          type = "app";
          program =
            let
              script = pkgs.writeShellScript "update-hash" ''
                echo updating-hash
                set -eu
                old_hash=$(nix eval ".#processor.dependencies.outputHash" --raw)
                log=$(mktemp)
                trap 'rm -f "$log"' EXIT
                sed -i "s|$old_hash|${lib.fakeHash}|" flake.nix
                if nix build ".#processor.dependencies" --no-link -L >"$log" 2>&1; then
                  new_hash=${lib.fakeHash}
                else
                  new_hash=$(grep 'got:' "$log" | tail -n1 | awk '{print $2}') || true
                  if [ -z "$new_hash" ]; then
                    cat "$log" >&2
                    sed -i "s|${lib.fakeHash}|$old_hash|" flake.nix
                    exit 1
                  fi
                fi
                sed -i "s|${lib.fakeHash}|$new_hash|" flake.nix
                echo "changed hash from:$old_hash to:$new_hash"
              '';
            in
            "${script}";
        };
      });
}
