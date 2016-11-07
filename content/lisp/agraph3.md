title: AllegroGraph その3
date: 2016-11-07
tags: lisp,allegrograph

前回全然lispじゃなかった。

今回はACLから繋いでみます。


# 準備

## Allegro CL 10.0 Free Express Editionのインストール

* Macの場合X11とかGTKとか必要とありますが、slimeから使うならなくてもいけます

## mlispをつくる

* AllegroGraph clientはalispでは動かないのでmlispを作ります
* [faq](http://franz.com/support/faq/#s3q7)にあるので、いちどalispを起動してから次にコードを実行します

```lisp
(progn
  (build-lisp-image "sys:mlisp.dxl" :case-mode :case-sensitive-lower
          :include-ide nil :restart-app-function nil)
    (when (probe-file "sys:mlisp") (delete-file "sys:mlisp"))
      (sys:copy-file "sys:alisp" "sys:mlisp"))
```

* mlispができたらslimeの設定を通します

```
(setq inferior-lisp-program "/Applications/AllegroCLexpress.app/Contents/Resources/mlisp")
```

## AllegroGraphのポート指定

* AllegroGraphをVirtualboxで動かしていて、それにslimeから接続する場合、必要なポートは
    * 10035
    * SessionPort(random)
* SessionPortがランダムなのはポートフォワーディングする時に面倒なのでコンフィグで指定します
    * /home/franz/ag/lib/agraph.cfgが設定ファイルです
    * SessionPorts 8080-8081
    * みたいに適当なポートレンジを書き加えます
    * Virtualboxから該当ポートへのフォワーディングを指定してAllegroGraphを起動

## AllegroGraph Clientダウンロードする

* [ここ](http://franz.com/agraph/downloads/clients)からダウンロード(メールアドレスが必要)
* MacのExpress EditionはAllegro CL 10.0 (non-SMP) 32-bit Mac OS Xが対応します
* ダウンロード後、展開して終了

# 接続

* tuple-storeはその2で作ったcattoyを使います
* せっかくなのでprologで

```lisp
(load "/path/to/agraph-6.1.3-macosx86-client-lisp-acl10.0/agraph.fasl")
(in-package :db.agraph.user)
(enable-!-reader)

; (setf *default-ag-http-port* 10035) ; ポートフォワーディングで変えてる場合はここで指定
(open-triple-store "cattoy")
(triple-count)
; => 451

(require :prolog)
(use-package :prolog)

(register-namespace "sc" "http://schema.org/" :errorp nil)
(select (?x ?y ?z)
  (q- ?x !sc:name ?y))
; => (("http://www.cattoy.info/cats/1" "みけ" nil))

; こっちはattempt to call `#:q-/3' which is an undefined function.になって動かなかった
; なんでだろう
; (?- (q- ?x ?y ?z))
```

とりあえず問い合わせまでできるようになりました

今回と前回使用したcattoyのturtleをダウンロードできるようにしています。

https://www.cattoy.info/cattoy.ttl

SPARQL エンドポイント置くのはちょっとサーバーリソース的に面倒だったのでデータだけです。
