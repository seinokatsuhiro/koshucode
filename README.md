# 甲州計算機プレビュー版


甲州計算機はデータ処理のためのソフトウェアです。
SQL データベースのような関係モデルにもとづくデータ処理を、
データベース分野だけではなく、[関係型データ交換][rdi]
などにも適用できる基礎技術です。
甲州計算機は、甲州記法というデータ言語を処理するソフトウェアで、
その使い方が [甲州記法の ABC][japanese] という入門書に説明されています。

このソフトウェアは、まだ、開発が続けられており、
正式にリリースされた版はありません。
現段階で試してみたい方のために、
プレビュー版をこのページからダウンロードできます。

 - Windows 向けにビルド済みの [プログラム][preview] (zip)
   をダウンロードするか、

 - [ソースコード][master] (zip) をダウンロードして、
   自身でビルドしてください。


Windows で甲州計算機を使うには
------------------------------------------------------------------

Windows 向けのビルド済み [プログラム][preview] をダウンロードすると、
zip ファイルのなかに以下のファイルが含まれています。

| ファイル       | 説明                                |
|----------------|-------------------------------------|
| LICENSE        | [3 条項 BSD ライセンス][bsd]        |
| README.md      | このファイル                        |
| SAMPLE.k       | koshu SAMPLE.k で実行できるサンプル |
| koshu-cmd.bat  | koshu.exe へのパスが設定された cmd  |
| koshu.exe      | 甲州計算機                          |

これをダウンロードされた方は、
[`koshu-cmd.bat`][koshu-cmd] をダブルクリックで起動してみてください。
左上に `The Koshucode Baala Implementation 0.103.0.0`
のようなバージョンが表示されていれば、使える状態になっています。
`koshu-cmd.bat` は、甲州計算機 `koshu.exe` へのパスを設定して [`cmd`][cmd] を起動します。
コマンド・プロンプトに対して、`koshu SAMPLE.k` というコマンドを入力すると、
甲州計算機がファイル `SAMPLE.k` を処理した結果が出力されます。

`koshu.exe` は、外部のファイルを参照せずに単体で動作するので、
`koshu.exe` と、必要であれば `koshu-cmd.bat` を別のディレクトリにコピーしても使えます。
[甲州記法の ABC][japanese] に掲載された例をを実行するには、
[draft / japanese / section][section] に
`koshu.exe` と `koshu-cmd.bat` をコピーして実行するとよいでしょう。


[master]:    https://github.com/seinokatsuhiro/koshucode/archive/master.zip
[preview]:   https://github.com/seinokatsuhiro/koshucode/archive/preview.zip
[japanese]:  https://github.com/seinokatsuhiro/abc-of-koshucode/tree/master/draft/japanese
[section]:   https://github.com/seinokatsuhiro/abc-of-koshucode/tree/master/draft/japanese/section
[english]:   https://github.com/seinokatsuhiro/abc-of-koshucode/tree/master/draft/english
[rdi]:       https://github.com/seinokatsuhiro/koshucode-design/tree/master/note/rdi
[cmd]:       http://ja.wikipedia.org/wiki/Cmd.exe
[bsd]:       http://ja.wikipedia.org/wiki/BSDライセンス
[koshu-cmd]: koshu-cmd.bat

