[![Scala CI](https://github.com/NakajoLab/B4-Processor/actions/workflows/scala.yml/badge.svg)](https://github.com/NakajoLab/B4-Processor/actions/workflows/scala.yml)

# B4SMT コア

Chisel で記述された、アウトオブオーダ実行と同時マルチスレッディングに対応する RISC-V プロセッサです。

[英語版のドキュメント](README.md)

## 必要なソフトウェア

プロセッサの生成、テスト用プログラムのビルド、開発環境の利用には [Nix](https://nixos.org/) を使用します。
インストールと設定については [Zero to Nix](https://zero-to-nix.com/) を参照してください。

利用可能なパッケージの再ビルドを避けるため、このプロジェクトのバイナリキャッシュを有効にしてください。

```sh
nix shell "nixpkgs#cachix" -c cachix use b4smt
```

Mill のコマンドを実行する前に、開発環境に入ってください。

```sh
nix develop
```

この環境には Java、[Mill](https://mill-build.org/)、[LLVM CIRCT](https://circt.llvm.org/)
（SystemVerilog 生成用の `firtool`）、Verilator、形式検証用のツールが含まれています。
開発には [IntelliJ IDEA](https://www.jetbrains.com/idea/) も利用できます。

## プロセッサの生成

次のコマンドで、`./processor` に SystemVerilog のソースを生成します。

```sh
make processor
```

## テスト用プログラムのビルド

次のコマンドで、サンプルプログラムと上流の [riscv-tests](https://github.com/riscv-software-src/riscv-tests)
をビルドし、`./programs` に配置します。

```sh
make programs
```

テスト用プログラムのツールチェーンとビルド依存関係は Nix で管理しています。

## テストの実行

`Slow` タグの付いた時間のかかるテストを除いて実行するには、次のコマンドを使用します。

```sh
mill b4smt.test.testOnly '*' -- -l org.scalatest.tags.Slow
```

時間のかかるテストを含め、無視されていないすべてのテストを実行するには、次のコマンドを使用します。
実行には長い時間がかかることがあります。

```sh
mill b4smt.test.testOnly '*'
```

プロセッサのプログラム実行テストには、後述する再利用可能な単体 Verilator ハーネスを使用します。
ユニット単位のシミュレーションには ChiselSim、形式検証には SymbiYosys を使用します。
コンパイル時間とディスク使用量を抑えるため、通常は波形を出力しません。
ChiselSim のテストで VCD 波形を出力する場合は、ScalaTest に `-DemitVcd=1` を渡してください。
このオプションは Mill や JVM のオプションではなく、`--` の後に指定します。

```sh
mill b4smt.test.testOnly '*' -- -l org.scalatest.tags.Slow -DemitVcd=1
```

このオプションは ChiselSim のテストに適用され、単体ハーネスには適用されません。
`testOnly` で個別のテストスイートを選択したり、IntelliJ IDEA からテストを実行したりすることもできます。

## 単体シミュレータ

プロセッサのプログラム実行、RISC-V 命令、ベンチマーク、パラメータ別のシミュレーションテストは、
既存の `B4SMTCoreWithMemory` テスト回路を包む、再利用可能な実行ファイルを使用します。
ユニット単位のシミュレーション、形式検証、回路生成のテストには変更ありません。
ハードウェアのパラメータは JSON で指定します。`simulator/config.json` はサンプル設定で、
スレッド数とデコーダ数はそれぞれ 1、`tagWidth=4`、`debug=true` です。
設定を変更するとハードウェアを再ビルドしますが、プログラムイメージとテスト手順は実行時に指定します。

```sh
nix build .#simulator --no-link
nix run .#simulator -- --program /path/to/branch.hex --max-cycles 200 --expect-register 13 20
nix run .#simulator -- --program /path/to/fibonacci.hex --max-cycles 400 --expect-register 6 55
nix build .#checks.x86_64-linux.simulator --no-link
```

生成用のコマンドには、個別のパラメータ用オプションではなく JSON ファイルを渡します。

```sh
mill --no-daemon --offline b4smt.test.runMain b4smt.simulator.StandaloneSimulator --config simulator/config.json --target-dir rtl
```

`nix/b4smt_simulator.nix` は、`pkgs.callPackage` で呼び出す際に
`configFile ? ../simulator/config.json` も受け取ります。
`Parameters` ケースクラスのすべてのフィールド名を指定でき、未知の名前を指定するとエラーになります。
省略したフィールドには `Parameters()` の既定値を使用します。
要素数や幅を表すフィールドには、Int の範囲内の正の JSON 整数を指定してください。
`debug` と `enablePExt` には JSON の真偽値を指定します。
レジスタ確認用のラッパーには `debug=true` が必要です。
`instructionStart` には、2^53-1 以下で正確に表現できる非負の整数、
または正の Long の範囲に収まる `0x` 形式の 16 進文字列を指定できます。
アドレスは 8 バイト境界に整列し、512 MiB のテスト用メモリを配置できるアドレス空間を確保する必要があります。
それ以外のアーキテクチャ上の制約は、既存の回路生成時の検査に従います。
C++ 側のインターフェースはスレッド数が変わっても共通です。
`--thread N` で確認するスレッドを選択できます。既定値は 0 で、範囲外の番号はエラーになります。

入力には既存のサンプルプログラムの出力（`nix build .#riscv-sample-programs`）を利用できます。
既定の入力形式は、空白で区切られた 2 桁の 16 進数による**バイト単位のデータ**です。
小文字の `x` 接頭辞は省略可能ですが、アドレス指定やコメントは記述できません。
`--format words` は `.64.hex` のワード列を、`--format binary` は生のバイト列を読み込みます。
バイト列はリトルエンディアンの 64 ビットワードにまとめ、最後のワードの不足分をゼロで埋めます。
その後、テスト回路のローダーに転送します。プログラムを RTL に組み込んでコンパイルするわけではありません。

起動のたびに新しいプロセスを作成し、生成された初期化処理を実行してから 10 サイクルのリセットを行い、
テスト回路と同じ手順でプログラムを読み込みます。
`--max-cycles` は、サンプルテストの制限と同じく、**読み込み完了後**のサイクル数を表します。
選択したレジスタが期待する符号なし 64 ビット値に到達すると成功になります。
値は 10 進数または `0x` 形式で指定できます。これはプログラム終了通知の規約ではありません。
タイムアウト、値の不一致、不正な入力、RTL のアサーション違反では、終了コードが非ゼロになります。
UART 出力は受信して表示し、UART 入力は非アクティブに固定します。
アサーションと検証レイヤーは有効なままで、不定値の扱いを簡略化する高速化オプションや、
アサーションを無効化するオプションは使用しません。
簡易動作確認では、**同じ実行ファイル**を別々のプロセスとして起動し、
branch（x13=20、200 サイクル）、fibonacci（x6=55、400 サイクル）、意図的なタイムアウトを確認します。

### テスト手順ファイル

`--plan FILE` を指定すると、順序付きのテスト手順全体を C++ 側で実行します。
サイクルごとに Scala とプロセス間通信する必要はありません。
単純なコマンドライン実行とは異なり、手順ファイルを使用する場合は最初に UART の ready を低く保ちます。
バイトを読み取る際にだけ ready を 1 サイクル上げ、読み取り後に下げます。
テストスイートでは、途中のレジスタ値の順序付き確認と、指定サイクルでの値の確認を維持しています。

たとえば、`csrtest.hex` の読み込み後に次の手順を実行します。

```text
wait 0 17 10 500
wait 0 17 20 500
wait 0 17 30 500
step 20
```

1 行に 1 つの操作を記述します。数値は符号なしの 10 進数または 16 進数です。

- `step CYCLES`：指定したサイクル数だけ進めます。`expect THREAD REGISTER VALUE`：レジスタ値を直ちに確認します。
- `wait THREAD REGISTER VALUE TIMEOUT`：制限サイクル数以内にレジスタが指定値になるまで待ちます。
- `change THREAD REGISTER VALUE TIMEOUT`：レジスタが**非ゼロ**になるまで待ち、その場で VALUE と比較してから 3 サイクル進めます。VALUE になるまで待つ操作ではありません。
- `ready BOOL`：UART の ready を設定します。`uart TIMEOUT PRINT BYTE`：期待するバイトを受信します。`any TIMEOUT PRINT`：任意のバイトを受信します。PRINT は表示の有無を指定します。
- `search TIMEOUT PRINT MAX_CHARACTERS LENGTH BYTE...`：不一致で照合位置を先頭に戻す、文字数制限付きの検索です。`suffix` は代わりに末尾の文字列を照合します。
- `dump`：レジスタを表示します。`monitor CYCLES WRITE_ADDRESS`：指定した書き込みアドレスを監視します。
- `stats "PATH" "JSON_PREFIX"`：全スレッドについて、レジスタ x6/x5 から求めた IPC を追記します。

Scala 側の `StandaloneTest` は、不変の `Parameters` 全体をシリアライズし、
テスト用 JVM 内で設定ごとに一度だけ、必要になった時点で既存の生成処理と Verilator を呼び出します。
各テストは新しい実行ファイルのプロセスとして起動します。
Mill や Nix を内部から起動したり、ChiselSim に切り替えたりすることはありません。
Verilator、C++ コンパイラ、make、firtool が必要なため、テストは開発環境内で実行してください。
ビルド結果、手順ファイル、テスト名、プロセスのログは `test_run_dir/standalone/jvm-*` に保存します。
`B4SMT_SIM_JOBS` でネイティブビルドの並列数を設定できます。既定値は 2 です。
サブプロセスの実時間の上限は、`B4SMT_SIM_BUILD_TIMEOUT_SECONDS`（既定値 1800 秒）と
`B4SMT_SIM_RUN_TIMEOUT_SECONDS`（既定値 3600 秒）で指定できます。値は正の秒数にしてください。
タイムアウトや割り込みが発生した場合は、対象のサブプロセスとその子孫プロセスを終了します。
これらの時間制限は、テスト手順のサイクル数制限とは別のものです。
必要に応じて、`bin/b4smt-sim` と `share/b4smt-simulator/config.json` を含む
パッケージのディレクトリを `B4SMT_SIMULATOR_DIR` に指定できます。
これらのファイルは `.#simulator` によってインストールされ、設定が一致しなければ明示的にエラーになります。
JVM の起動をまたぐコンパイルキャッシュはありません。

## フォーマット

Scala と Mill のソースは Mill の Scalafmt タスクで、Nix ファイルは treefmt を通して整形します。

```sh
nix fmt
```

## トラブルシューティング

### 依存関係の変更後にプロセッサを生成できない

Nix の依存関係キャッシュが古いためにコンパイルが失敗する場合は、次のコマンドでハッシュを更新してください。

```sh
nix run .#update-hash
```

このコマンドは、`flake.nix` 内の依存関係キャッシュのハッシュを更新します。
