title: インターネットにつながらないLinuxクラスターを運用する
date: 2017-09-16
tags: server

# はしがき

ここ最近は小規模なLinuxクラスターの運用管理もやっています。

事情よりクラスター自体がインターネットに接続できないため、通常に比べ管理が面倒だったり、外部サービスを利用できないため不便なことが多々あります。
そんな環境で色々やってるので書いておきます。

環境はCentOS7です。

# 構成管理

ansibleを使用しています。クライアント側にインストールの必要が無いため、管理用端末のみインターネットに接続してインストールすればいいので楽です。

ansible用ユーザーを作って公開鍵認証にしていますが、初回のユーザー作成と鍵配布もansibleで行うため、
そのときだけは`remote_user: root`と`become_method: su`を使ってrootパスワードを使用しています。


```yaml
---
- hosts: all
  become: yes
  become_method: su
  remote_user: root
  tasks:
    - name: provisioner
      user:
        name: provisioner
        append: yes
    - name: provisioner ssh key
      authorized_key:
        user: provisioner
        key: "{{ lookup('file', '../../ssh/id_rsa.pub') }}"
    - name: add sudoers
      lineinfile: >
        dest=/etc/sudoers
        line='provisioner ALL=(ALL) NOPASSWD: ALL'
```

上記のplaybookを`--ask-become-pass`, `--ask-pass`付きで流します。

これ以後はprovisionerユーザーがsudo可能になるので、
他のplaybook`become:yes`だけでシステム設定できるようになりました。

ちなみにこのときにansible.cfgで`ssh_args`でssh鍵を指定していると接続エラーになるので、
手動でコメントアウトしています。いい方法ないかな。

```ansible.cfg
[ssh_connection]
ssh_args = -F ssh/ssh_config #ここ
```

# package

## yum

インターネットにつながらないため、yumなどがそのままでは使用できません。
rpmをコピーする方法では依存関係処理が面倒なため、フルミラーを作ります。
ftp.riken.jpあたりからrsyncしてくるのが早いです。

yumミラーはhttpで見えるようにしてもいいのですが、今回は全ノードがnfsをマウントしているのでそこにおいています。

/etc/yum.repos.dの中身を消して、次のようにローカルのものに書き換えれば完了です。

```
 [local-repo-os]
name=local-repo-os
baseurl=file:///nfs/mirror/centos/7/os/x86_64
enabled=1

[local-repo-updates]
name=local-repo-updates
baseurl=file:///nfs/mirror/centos/7/updates/x86_64
enabled=1
```

## R

yum以外にもRのリポジトリミラーも作っています。

Rの場合、rsyncでコピー後、Rからwrite_PACKAGE関数を読んでパッケージリストを作る必要があります。

```R
tools:::write_PACKAGES("/nfs/mirror/R/src/contrib")
```

その後、$HOME/.Rprofileにmirrorの場所を指定すると通常どおり`install.packages`が使用できるようになります。


```R
options(repos="file:///nfs/mirror/R")
```

[bioconductor](https://www.bioconductor.org/about/mirrors/mirror-how-to/)もミラーしているのですが、ディレクトリ構成が異なりcontribディレクトリの場所を`install.packages`が見つけてくれないため、上記のoptionsではうまく行きません。

今のところ`contriburl`を直接指定していますが、もっといい方法がありそうです。

```R
install.packages(“package”, contriburl=”file:///nfs/mirror/biocondutor/packages/3.5/contrib”)

```

### stringi

ネットワークにつながらない環境で、`tidyverse`などをインストールしようとすると大抵stringiのインストールで失敗します。
stringiがicudtNN.zip(NNは数字)をインストール中にダウンロードしようとするためです。

[http://static.rexamine.com/packages/](http://static.rexamine.com/packages/)から対応バージョンのzipファイルをダウンロードして、ノードにコピーします。

`configure.vers`に`ICUDT_DIR`をzipのあるパスを指定してインストール可能です。

```R
install.packages("stringi", configure.vars="ICUDT_DIR=<dir_to_copy_icudt_from>")
```

## python

直接whlファイルをpipで入れるときにプラットフォームが合わないとエラーになることがあります。

whlフィアルの名前が`-cp27m-manylinux1_x86_64.whl`のようにmanylinuxとなっていたら

```python
import pip; print(pip.pep425tags.get_supported()
```

とやって出て来るプラットフォーム名に合わせるとインストール可能になります。

pythonのミラーは今後の課題です。

# ntp

結構忘れがちなのがノードの時計です。ほっとくと結構ずれますが、publicなntpサーバーには接続できないため補正がかかりません。
内部ntpサーバーを立ればクラスター内の時刻同期は出来ますが、外部とはずれ続けます。その時刻の補正も必要です。

対策として、[シチズンGPSタイムサーバー](http://tic.citizen.co.jp/timeserver/)を使っています。
アンテナを窓際において動かしていますが、 幸い受信感度は良く、問題なく時刻合わせが出来ています。

# 監視

外部監視サービスは使用できないので、自前で運用することになります。

今回は[prometheus](https://prometheus.io)を使い、
グラフはgrafanaで表示しています。

prometheusを選んだ理由は、触ってみたかったというのが一番ですが

* 単一バイナリで楽
* アラートもできる

という点からも使ってみようという気になりました。

service discoveryは使用してないためあまり恩恵がないかもしれませんが、
規模が大きくなったらconsulあたりを入れようかとも考えています。

使ってみた感想としては、

* node_exporter楽
* Textfile Collectorで追加も楽
* queryやconfigのドキュメントが最小限なのでなれるまで時間かかる
* わかればgrafanaからグラフ作るのも色々できる
* alert.rulesの書式がいつの間にかymlになってて動かなくなってた

zabbixのようになんでも入りではありませんが、手をかけることで育っていく感じがしています。


# アラート

アラートルールは

* ノードダウン
* RAID状態(textfile collectorで自前で出してる数値)
* DISK空き容量
* CPU温度

などをとりあえず設定しています。雑です。

```yaml
groups:
- name: alert.rules
  rules:
  - alert: InstanceDown
    expr: up == 0
    for: 2m
    labels:
      severity: critical
    annotations:
      description: '{{ $labels.instance }} has been down for more than 2 minutes.'
      summary: Instance {{ $labels.instance }} down
  - alert: RAIDFail
    expr: raid_failed + raid_degraded > 0
    labels:
      severity: critical
    annotations:
      description: '{{ $labels.instance }} raid failed'
      summary: Instance {{ $labels.instance }} raid failed
  - alert: DiskSpace
    expr: min(node_filesystem_free / node_filesystem_size) < 0.05
    labels:
      severity: critical
    annotations:
      description: '{{ $labels.instance }} disk full'
      summary: Instance {{ $labels.instance }} disk full
  - alert: CPUTemp
    expr: node_hwmon_temp_celsius{chip=~"platform_coretemp_.*"} > 80
    labels:
      severity: critical
    annotations:
      description: '{{ $labels.instance }} cpu over temp'
      summary: Instance {{ $labels.instance }} cpu over temp
```

## 通知

アラートできるようになりましたが、インターネットにつながらない場合アラートを送る先が難しいです。
slack通知などができれば楽なのですがそういうわけにもいきません。
grafanaのダッシュボードをずっと見ててもいいのですがあまり楽しくはないです。

このクラスターは自分たちが使用しているため、24時間365日の監視は必要なく、
アラートは業務中に自分が気がつけばいいだろうということにしました。

alertmanagerから任意のwebhookを呼べるので、
今回は使用しているStumpWM内にwebサーバーを動かし受信、メッセージを表示させてみました。

.stumpwm/init.lispの初期化コード内でwookieを立てて、
alertmanagerから送られてくるjsonをそのまま表示しています。

```lisp
(ql:quickload 'wookie)
(ql:quickload 'cl-json)
(wookie:load-plugins)
(wookie:defroute (:post "/") (req res)
  (let ((body (wookie-util:body-to-string
                (wookie:request-body req)
                (wookie-util:get-header (wookie:request-headers req) "context-type")))
	      (*print-right-margin* 30))
    (stumpwm:message "ALERT~%~W" (cl-json:decode-json-from-string body)))
  (wookie:send-response res :body "OK"))
(bt:make-thread 
  (lambda ()
    (as:with-event-loop ()
      (wookie:start-server
        (make-instance 'wookie:listener :port 9999))))))
```

お手軽ですが、まあ気がつくだろうという感じです。

余談ですがアラートをテストしていて、 アラートが動いてないことに気がつくためのアラートはどうしたらいいんだろうと考えています。

# まとめ

インターネットにつながらないと、色々なエコシステムに乗ることが出来ず面倒です。
自前で運用するリスクやコストもかかるので**可能なら**避けたほうが良いでしょう。

10年ぐらい前の運用はこんな感じだった気もするので、世の中便利になったのではないでしょうか。
AWSとかdockerとかすごいなあと思いました。

他にも思いついたら書いていきたいと思います。