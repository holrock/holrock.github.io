title: AllegroGraph その1
date: 2016-11-03

AllegroGraphを動かしてみます。

* [http://franz.com/agraph/downloads/](http://franz.com/agraph/downloads/)からAllegroGraph Virtual Machineをダウンロード
* 展開してVirtualboxから読み込む
  * Virtualboxで仮想マシンを新規作成
    * type linux
    * version ubuntu 64bit
    * ディスク作成時にすでにあるディスクを使用するからAllegroGraph 6.0 Virtual Machine.vmdkを選択
    * ネットワークのポートフォワーディングでホストポート10034(なんでもいい)からゲストポート10035へフォワーディング設定する
* VMを起動
  * franz/allegrographでログイン
  * デスクトップにあるStart AGを起動する
* ホストマシンからブラウザでlocalhost:10034へアクセスする
* AllegroGraph WebViewへアクセスできたら、User anonymousのところからtest/xyzzyでログイン
* [Quick-Start](http://franz.com/agraph/support/documentation/current/agraph-quick-start.html)にしたがって
  * Create new repositoryにkennedyと入力しcreate
  * http://localhost:10034/repositories/kennedyを開く
  * from a server-side fileを押してfileのところから
    * ..で上に上がっていって、最終的に/home/franz/ag/tutorial/kennedy.ntriplesを選択
    * OKで取り込まれる
  * Queriesから
    * `select ?p ?o WHERE {<http://www.franz.com/simple#person1> ?p ?o}`クエリ投げてみて結果が見えれば成功
* 試しに他のデータも投入してみる
  * 新しくrepositoryを作って
  * [linkdata.org](http://linkdata.org/home)から適当なデータを持ってくる
    * 今回はこれを試した [就活イベント](http://linkdata.org/work/rdf1s4668i/syukatsu_api.html)
    * [rdf/xml](http://linkdata.org/api/1/rdf1s4668i/syukatsu_rdf.xml)を保存
    * WebViewのfrom an uploaded fileからファイルを選択、フォーマットをRDF/XMLにしてOK
    * 取り込まれるもののPのところがエスケープされたまま入ってしまう(%E4%BC%9A%E5%A0%B4\_みたいな)

今回はここまで
