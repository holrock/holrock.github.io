title: rub2を公開しました
date: 2017-12-23
tags: ruby

[github](https://github.com/holrock/rub2)
[gem](https://rubygems.org/gems/rub2)

rub2はtorque qsub用ruby DSLです。

簡単なjobを簡単に書くことを目的にデザインされています。

rubyでtorque jobを書く方はそういないと思いますが、お役に立てば幸いです。

簡単なサンプルです。

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

その後、各jobや、qstatの状態を監視し、すべてのjobが終了するまで待ち合わせます。
