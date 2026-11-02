[![Scala CI](https://github.com/NakajoLab/B4-Processor/actions/workflows/scala.yml/badge.svg)](https://github.com/NakajoLab/B4-Processor/actions/workflows/scala.yml)

# B4SMT Core

An out-of-order, simultaneous-multithreading RISC-V processor written in Chisel.

[Japanese documentation](README.ja.md)

## Requirements

Install [Nix](https://nixos.org/) to generate the processor, build test programs,
and enter the development environment. See [Zero to Nix](https://zero-to-nix.com/)
for installation and setup instructions.

Enable the project's binary cache to avoid rebuilding available packages:

```sh
nix shell "nixpkgs#cachix" -c cachix use b4smt
```

Enter the development environment before running Mill commands:

```sh
nix develop
```

The environment provides Java, [Mill](https://mill-build.org/),
[LLVM CIRCT](https://circt.llvm.org/) (`firtool` for SystemVerilog generation),
Verilator, and the formal-verification tools. You can also use
[IntelliJ IDEA](https://www.jetbrains.com/idea/) for development.

## Generate the processor

Generate SystemVerilog sources under `./processor`:

```sh
make processor
```

## Build test programs

Build the sample programs and upstream [riscv-tests](https://github.com/riscv-software-src/riscv-tests)
under `./programs`:

```sh
make programs
```

Nix manages the test-program toolchains and build dependencies.

## Run tests

Run the quick suite, excluding tests tagged `Slow`:

```sh
mill b4smt.test.testOnly '*' -- -l org.scalatest.tags.Slow
```

Run all non-ignored tests, including the slow tests (this can take a long time):

```sh
mill b4smt.test.testOnly '*'
```

Processor-program tests use the reusable standalone Verilator harness described
below. Unit simulations use ChiselSim, and formal checks use SymbiYosys.
Waveforms are disabled by default to reduce compilation time and disk use.
For ChiselSim tests, enable VCD output with the ScalaTest option `-DemitVcd=1`
(after `--`, not as a Mill or JVM option):

```sh
mill b4smt.test.testOnly '*' -- -l org.scalatest.tags.Slow -DemitVcd=1
```

This option applies to ChiselSim tests, not the standalone harness. You can also
select individual test suites with `testOnly` or run tests from IntelliJ IDEA.

## Standalone simulator

The processor program, RISC-V, benchmark and parameter simulation suites use
this reusable executable around the existing `B4SMTCoreWithMemory` fixture.
Unit simulations, formal checks and elaboration tests are unchanged. Hardware
parameters come from JSON: `simulator/config.json` is the sample configuration
(one thread, one decoder, `tagWidth=4`, `debug=true`). Config changes rebuild
hardware; program images and test action plans remain runtime inputs.

```sh
nix build .#simulator --no-link
nix run .#simulator -- --program /path/to/branch.hex --max-cycles 200 --expect-register 13 20
nix run .#simulator -- --program /path/to/fibonacci.hex --max-cycles 400 --expect-register 6 55
nix build .#checks.x86_64-linux.simulator --no-link
```

The generator CLI takes JSON, not individual parameter flags:

```sh
mill --no-daemon --offline b4smt.test.runMain b4smt.simulator.StandaloneSimulator --config simulator/config.json --target-dir rtl
```

`nix/b4smt_simulator.nix` also accepts `configFile ? ../simulator/config.json`
when called with `pkgs.callPackage`. All `Parameters` case-class field names are
accepted; unknown names fail. Omitted fields use `Parameters()` defaults.
Dimension fields must be positive JSON integers fitting Int; `debug` and
`enablePExt` must be JSON booleans. The register-check wrapper requires
`debug=true`. `instructionStart` accepts an exact nonnegative integer up to
2^53-1 or a `0x` hex string fitting positive Long, must be 8-byte aligned, and
must leave address space for 512 MiB of fixture memory. Other architectural
constraints are left to existing RTL elaboration checks. The C++ interface is
stable across thread counts; `--thread N` selects the checked thread (default 0),
with out-of-range indices rejected.

Use the existing sample-program outputs (`nix build .#riscv-sample-programs`).
Input defaults to **byte-oriented hex**, whitespace-separated two-digit hex bytes,
with optional lowercase `x` prefix; no addresses/comments. `--format words`
loads `.64.hex` words and `--format binary` loads raw bytes. Bytes
are packed little-endian into 64-bit words and the final word is zero-padded,
then streamed through the fixture's loader, not compiled into the RTL.

Each invocation starts fresh, evaluates emitted initial blocks, resets for ten
cycles, and follows the fixture's load protocol. `--max-cycles` counts cycles
**after loading**, matching the sample test bounds. The process passes as soon
as the selected register reaches the expected unsigned 64-bit value (decimal
or `0x` notation); this is not a program-exit protocol. Timeout/wrong value,
invalid input and RTL assertion failures exit nonzero. UART output is consumed
and printed; UART input is tied inactive. Assertions and verification layers
remain enabled; no fast-X or assertion-disabling options are used. The smoke
check runs branch (x13=20, 200 cycles), fibonacci (x6=55, 400 cycles), and an
expected timeout using the **same executable**, in separate processes.

### Test action plans

`--plan FILE` runs an entire ordered test in C++, without per-cycle Scala IPC.
Unlike the simple CLI, plans initially hold UART ready low; byte reads assert
ready for exactly one consumption cycle and then lower it. The suites retain
sequential intermediate register checks and fixed-cycle expectations.

For example, after loading `csrtest.hex`:

```text
wait 0 17 10 500
wait 0 17 20 500
wait 0 17 30 500
step 20
```

One action per line, unsigned decimal/hex numbers:
- `step CYCLES`; `expect THREAD REGISTER VALUE`
- `wait THREAD REGISTER VALUE TIMEOUT`
- `change THREAD REGISTER VALUE TIMEOUT`: wait for **nonzero**, immediately
  expect VALUE, then step three cycles (not a wait for VALUE)
- `ready BOOL`; `uart TIMEOUT PRINT BYTE`; `any TIMEOUT PRINT`
- `search TIMEOUT PRINT MAX_CHARACTERS LENGTH BYTE...`: fixture's bounded,
  reset-on-mismatch search; `suffix` uses a sliding suffix instead
- `dump`; `monitor CYCLES WRITE_ADDRESS`
- `stats "PATH" "JSON_PREFIX"`: append register x6/x5 IPCs for every thread

Scala's `StandaloneTest` serializes the complete immutable `Parameters`, lazily
calls the existing generator and Verilator once per configuration in a test JVM,
and launches a fresh executable process per test. It never invokes Mill/Nix or
falls back to ChiselSim. Run tests inside the devshell (Verilator, C++ compiler,
make and firtool must be available). Builds, plans, test names and process logs
are retained under `test_run_dir/standalone/jvm-*`. `B4SMT_SIM_JOBS` controls
native build parallelism (default 2). Subprocess wall-clock limits are controlled
by `B4SMT_SIM_BUILD_TIMEOUT_SECONDS` (default 1800) and
`B4SMT_SIM_RUN_TIMEOUT_SECONDS` (default 3600); values must be positive seconds.
Timed-out or interrupted subprocesses and their descendants are terminated.
These limits are separate from the plan's cycle bounds.
Optionally `B4SMT_SIMULATOR_DIR` names a
package directory containing `bin/b4smt-sim` and
`share/b4smt-simulator/config.json` (as installed by `.#simulator`); a configuration
mismatch fails explicitly. There is no cross-JVM compilation cache.

## Continuous integration

GitHub Actions runs `nix flake check` on x86-64 Linux.
Checks cover the quick test suite, standalone simulator smoke tests, formatting,
processor generation, test-program builds, and the development shell. Slow tests
are not included. Generated processor sources are uploaded as a CI artifact.

CI uses the niks3 cache at `https://niks3.s.ihavenojob.work` with GitHub OIDC;
no `CACHIX_AUTH_TOKEN` secret is needed. Uploads require the cache server to
allow this repository's OIDC identity. Fork pull requests use read-only caching.

## Formatting

Format Scala and Mill sources through Mill's Scalafmt task, and Nix files with
treefmt:

```sh
nix fmt
```

## Troubleshooting

### Processor generation fails after dependency changes

If compilation fails because Nix's dependency cache is out of date, update its
hash:

```sh
nix run .#update-hash
```

This updates the dependency-cache hash in `flake.nix`.
