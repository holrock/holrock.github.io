title: OPAM 2 local mirrorの作り方
date: 2019-02-27
tags: ocaml

インターネットに接続できないクラスター用リポジトリミラー作成シリーズのOPAM版です。

opam1の頃はopam-mirrorを使って、しこしこ作ってましたが、opam2から楽になりました
(https://discuss.ocaml.org/t/how-to-setup-local-opam-mirror/4423)


作業環境にはopamインストールして、リポジトリのファイルを全てダウンロードします。

```sh
$ git clone https://github.com/ocaml/opam-repository.git
$ cd opam-repository
# パッケージをダウンロードする
$ opam admin cache
# indexファイルをつくる
$ opam admin idnex
```

次はミラーサーバーにcacheとopam-repositoryをコピーして、httpdから見られるようにします。
ここの設定は省略します。
内部ノードから、`http://mirror-server/opam-repository`, `http://mirror-server/opam/cache`でアクセスできるものとします。

内部ノードにopamの実行ファイルをコピーします。パスも通して使えるようにしておきます。

```sh
# base-compilerのビルドでエラー出るが無視
$ opam init http://mirror-server/opam-repository
```

```sh
$ vi ~/.opam/config
# archive-mirrors: [ "http://mirror-server/opam/cache" ]を追加l(pathはコピーしたところ)
# コンパイラを入れる
$ opam switch create 4.09
```

これで完了です。あとはopamから好きなパッケージをインストール出来ます。

インストールしたパッケージのドキュメントがみたい場合は
`opam odig`で`odig`をインストール後、`odig odoc`でドキュメントを作成。
ブラウザが使用できれば、そのまま`odig`で見ることができます。

ブラウザが使用できない環境では`$(odig cache path)/html/$1/index.html`にhtmlファイルができるので、
次のようなスクリプトを作って見ることができます。とりあえず`wodig`として保存します。

```sh
#!/bin/sh
w3m -s -o confirm_qq=false "$(odig cache path)/html/$1/index.html"
```

`wodig odig`と実行します。