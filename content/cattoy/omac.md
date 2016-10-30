title: cattoyに画像proxyを入れました
date: 2016-10-31

cattoy.infoでは外部画像を結構な数表示しています。
外部画像にはhttpでしかアクセスできないものもあり、せっかくwww.cattoy.infoが
https接続でも警告が出てしまいます。


ちょうど、次の記事
[Camoで実現するセキュアな画像プロキシサーバ](http://qiita.com/MintoAoyama/items/6cd71b84e6225f86f819)
を見かけたので

camoクローンをocamlで作ってみました。
[https://github.com/holrock/omac](https://github.com/holrock/omac)

まだおもちゃみたいなものですが、一応本番で動いています。

構成は https -> nginx(cacheもここで) -> http -> omac -> [画像server]

今のままだとomacが落ちてもnginx側のキャッシュが有効で気がつかないことがあるようなので、
そのうちなんとかしないと。

ほかにもデーモン化やログ管理なんかもやらないといけないのですが、なんとなく動いてしまうと満足してしまいますね。
