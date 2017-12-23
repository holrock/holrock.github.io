title: rub2を公開しました
date: 2017-12-23
tags: ruby

[github](https://github.com/holrock/rub2)
[gem](https://rubygems.org/gems/rub2)

rub2はtorque qsub用ruby DSLです。
簡単なjobを簡単に書くことを目的にデザインされています。

rub2を使うことで得られるメリットは

* jobの終了を待つ
* 多数のファイルなどに対するarry jobが簡単にかける
* 失敗したjobのリトライができる

などがあります。

何年か内部で使用していたツールですが、gem化のついでに公開しました。

rubyでtorque jobを書く方はそういないと思いますが、お役に立てば幸いです。

## 簡単なサンプル

```ruby
require 'rub2'

submit "SimpleJob" do
  execute_with Dir.glob("/etc/*.conf") do |file|
     "wc -l #{file}"
  end
end
```

と書くと、

```sh
wc -l nfs.conf
```

のようなjobが`Dir.glob("/etc/*.conf")`が返した配列の要素ごとに生成され実行されます。

その後、各jobやqstatの状態を監視し、すべてのjobが終了するまで待ち合わせます。

## あれこれ

* `execute_with` は配列(複数可)をに受取り、 blockの結果をbashスクリプトとして出力します。

* shellでエラーになる時や、事前に動作を確認したい時は`dry_run`を指定して実行すると、
実際にsubmitされるスクリプトが出力されるので、参考にしてください。

* node側にもrubyがインストールされている必要があります。

* torqueのバージョンによっては動かないことがあります。

