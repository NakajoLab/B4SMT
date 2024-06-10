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
* [sbt]
* [LLVM circt] (System Verilog生成用のfirtoolコマンドラインツール)

[intellij]: https://www.jetbrains.com/idea/
[sbt]: https://www.scala-sbt.org/
[LLVM circt]: https://circt.llvm.org/

テスト
* Verilator
* Icarus Verilog

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
$ sbt "testOnly * -- -l org.scalatest.tags.Slow"
```

すべてのテストを実行するには次のコマンドを実行します。（とても時間がかかります）
```shell
$ sbt test
```

細かく実行するテストを決めたい場合はIntellij IDEAでプロジェクトを開き、各テストを実行してください。

## トラブルシューティング
### 依存関係周りを編集したらプロセッサが生成できない

コンパイル中に依存関係周りで問題が発生していれば、Nixの依存関係のキャッシュに問題がある場合があります。
その場合は次のコマンドを実行してください。
```sh
$ nix run ".#update-hash"
```

このコマンドでnixの依存関係をを管理するHashを更新できます。
