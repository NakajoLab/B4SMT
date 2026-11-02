[![Scala CI](https://github.com/NakajoLab/B4-Processor/actions/workflows/scala.yml/badge.svg)](https://github.com/NakajoLab/B4-Processor/actions/workflows/scala.yml)

# B4SMT Core

OoOのプロセッサ

## 準備
これらのソフトウェアをインストールしておく必要があります。

### プロセッサの生成・テストコードの生成
* [Nix][Nix download]

Nixについては[Nix]の公式サイトや[Zero to Nix]を参考にしてみてください。

次のコマンドでこのプロジェクトのキャッシュを有効にしておくと多くのパッケージのビルドを省略できます。

```shell
$ nix shell "nixpkgs#cachix" -c cachix use b4smt
```

[Nix download]: https://zero-to-nix.com/start/install
[Nix]: https://nixos.org/
[Zero to Nix]: https://zero-to-nix.com/

開発
* [intellij IDEA][intellij]
* java
* [Mill]
* [LLVM circt] (System Verilog生成用のfirtoolコマンドラインツール)

[intellij]: https://www.jetbrains.com/idea/
[Mill]: https://mill-build.org/
[LLVM circt]: https://circt.llvm.org/

テスト
* Verilator（ChiselSim）
* SymbiYosys（形式検証）

## プロセッサの生成
次のコマンドで`./processor`にSystem Verilogのソースが出力されます。

```shell
$ make processor
```

## テストコードの生成
次のコマンドで`./programs`にいくつかテスト用のプログラムと[riscv-tests]のコンパイル結果が出力されます。

```shell
$ make programs
```

[riscv-tests]: https://github.com/riscv-software-src/riscv-tests

## テスト用のプログラムの生成
テスト用プログラムは`nix`を使って管理しています。

## テスト方法
いくつかの時間のかかるテストを除いたテストは次のように行います。
```shell
$ mill b4smt.test.testOnly '*' -- -l org.scalatest.tags.Slow
```

すべてのテストを実行するには次のコマンドを実行します。（とても時間がかかります）
```shell
$ mill b4smt.test.testOnly '*'
```

シミュレーションテストはChiselSimを直接使用し、形式検証はSymbiYosysで実行します。
通常はコンパイル時間とディスク使用量を抑えるため、VCD波形を出力しません。
必要な場合だけScalaTestに`-DemitVcd=1`を渡します（Mill/JVMのオプションではなく、`--`の後に指定）。
```shell
$ mill b4smt.test.testOnly '*' -- -l org.scalatest.tags.Slow -DemitVcd=1
```

細かく実行するテストを決めたい場合はIntellij IDEAでプロジェクトを開き、各テストを実行してください。

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
native build parallelism (default 2). Optionally `B4SMT_SIMULATOR_DIR` names a
package directory containing `bin/b4smt-sim` and
`share/b4smt-simulator/config.json` (as installed by `.#simulator`); a configuration
mismatch fails explicitly. There is no cross-JVM compilation cache.

## フォーマット
Scala・Millのソース（Mill経由のScalafmt）とNixファイルをまとめて整形します。
```shell
$ nix fmt
```

## トラブルシューティング
### 依存関係周りを編集したらプロセッサが生成できない

コンパイル中に依存関係周りで問題が発生していれば、Nixの依存関係のキャッシュに問題がある場合があります。
その場合は次のコマンドを実行してください。
```sh
$ nix run ".#update-hash"
```

このコマンドでnixの依存関係をを管理するHashを更新できます。
