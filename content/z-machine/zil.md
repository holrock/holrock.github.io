title: ZIL
date: 2016-12-04
tags: z-machine,lisp,zil

なんとなくZ-Machine周りについて調べていたので、ZILについてメモしておきます。

ZIL(Zork Implementation Language)はZorkなどで知られるZ-Machineで動作するInteractive Fiction(IF)を記述するための言語です。

まず、Z-Machine関係の用語を説明します。

## IF

IFはいわゆるテキストアドベンチャーゲームです。
テキストをコマンドとして入力して進行していくゲームで、
[zork](https://ja.wikipedia.org/wiki/ゾーク#.E3.82.B3.E3.83.9E.E3.83.B3.E3.83.89.E5.85.A5.E5.8A.9B)を
見るとどんなものかはすぐわかります。

## Z-Machine

Z-MachineはIFのための仮想マシンです。
Infocom社によって作られました。

多様なプラットフォームへの移植性を確保するため、Zork1などはZ-Machine上で動くようになっていました。

Z-MachineはZILをコンパイルして出力されるZ-codeと呼ばれるを命令を解釈して実行します。

Z-Machineの仕様は
[The Z-Machine Standards Document](http://inform-fiction.org/zmachine/standards/z1point1/index.html)
にまとまっていますが、フラグなどの扱いについて一部、[ztools](https://github.com/SamB/ztools)との相違が見られました。

## ZIL

ここから本題です。

ZILの情報は[ZIL Manual](http://www.ifwiki.org/index.php/ZIL)などにあります。

この後に出てくるソースコードは、ZIL Manualからの引用になります。

### BASIC

オリジナルのZorkは MIT Design Language(MDL)で書かれていたとあり、ZILにもその影響が強く見られます。

サンプルコードを見て見ましょう。
~~MDLと違い、式にはangle bracket (<>) だけが使われます。~~

ここ不正確だったので修正。ご指摘ありがとうございます。
フォームにangle bracket (<>), listに括弧が使われます。


```lisp
<* <+ 10 5> <- </ 26 2> 5>>
```

括弧さえ読み替えれば、みたまんまLISPですね。

~~丸括弧も使用されますが、グループ化のみに使用されているようです。~~

例えば、`cond` の条件と式には丸括弧が使用されます。
関数引数なども丸括弧が使われています。

```lisp
<COND (<if-this-is-true>
       <then-do-this>)>
```

ちなみに`<>`は`FALSE`を意味します。

次は関数定義を見て見ます。
関数定義には `ROUTINE` を使用します。

```lisp
<ROUTINE RHYME ("AUX" ARG1 ARG2)
      <SET ARG1 30>
      <SET ARG2 "September">
      <LINE-IN-RHYME .ARG1 .ARG2>
      <SET ARG1 28>
      <SET ARG2 "February">
      <LINE-IN-RHYME .ARG1 .ARG2>
      etc.>
<ROUTINE LINE-IN-RHYME (ARG-A ARG-B)
      <TELL N .ARG-A " days hath " .ARG-B "." CR>>
```

ここでは `RHYME` 、 `LINE-IN-RHYME` の二つのルーチンを定義しています。

`RHYME`の直後にある`("AUX" ARG1 ARG2)`はローカル変数宣言になります。`"AUX"`以降の`ARG1`, `ARG2`の二つの変数を宣言しています。
通常の引数と違い、呼び出し元から渡されるものではありません。

逆に`LINE-IN-RHYME`の`ARG-A`, `ARG-B`は呼び出し元から渡された引数になります。

後、目につくのは`.ARG1`の`.`ですね。これはローカル変数の値を参照する意味になります。

ちなみに、グローバル変数を参照する場合は`,`を使います。
次のコードは`PIZZA-EATEN`グローバル変数の値をインクリメントするコードです。

```
<SETG PIZZA-EATEN <+ ,PIZZA-EATEN 1>>
```

## ROOM, OBJECT, ROUTINE

ZILには大きく分けて3つの要素があります。

一つは先ほどのROUTINE。これはそのまま関数で、インプットに対するアクションを定義したり色々な用途に使用されます。

残りの二つは、ROOMとOBJECTです。

ROOMは、playerが移動したり調べたりすることができる場所のことです。今でいうLEVELに近いかもしれません。

OBJECTはそのままゲーム中に存在するオブジェクトです。
ROOMとOBJECTの定義方法は次の通りです。

```lisp
<ROOM LIVING-ROOM
     (LOC ROOMS)
     (DESC "Living Room")
     (EAST TO KITCHEN)
     (WEST TO STRANGE-PASSAGE IF CYCLOPS-FLED ELSE
          "The wooden door is nailed shut.")
     (DOWN PER TRAP-DOOR-EXIT)
     (ACTION LIVING-ROOM-F)
     (FLAGS RLANDBIT ONBIT SACREDBIT)
     (GLOBAL STAIRS)
     (THINGS <> NAILS NAILS-PSEUDO)>
```

```lisp
<OBJECT LANTERN
       (LOC LIVING-ROOM)
       (SYNONYM LAMP LANTERN LIGHT)
       (ADJECTIVE BRASS)
       (DESC "brass lantern")
       (FLAGS TAKEBIT LIGHTBIT)
       (ACTION LANTERN-F)
       (FDESC "A battery-powered lantern is on the trophy case.")
       (LDESC "There is a brass lantern (battery-powered) here.")
       (SIZE 15)>
```

これでLIVING-ROOMにLANTERNが置かれたことになります。

## Input, Praser, Handler

IFはユーザーからのインプットをパースしてアクションを起こします。たとえば

    >HIT UNCLE OTTO WITH THE HAMMER
    You knock some sense back into Uncle Otto, and he stops
    insisting that he's Napoleon Bonaparte.

`HIT UNCLE OTTO WITH THE HAMMER`がユーザーインプットで、2行目からが反応です。

ZorkシリーズなどInfocomのゲームは、上記のような文法をもった構文が使えたことで有名です。

パーサーはインプットを解析し、PRSA, PRSO, PRSIと呼ばれる要素へ分解します。

先ほどの例では次のようになります。

* PRSA -> HIT
* PRSO -> the UNCLE OTTO object
* PRSI -> the HAMMER object

分解が終わると、HandlerはPRSIオブジェクトのACTION ROUTINEを呼び出します。

PRSIのACTION ROUTINEで終了しなかった場合、次にPRSOのACTION ROUTINEが呼ばれ、
動詞のデフォルトアクションが呼ばれるまで連鎖します。

こうやってインプットをからゲーム内オブジェクトにインタラクションしていくことでゲームが進行していきます。

面白いのは、同じLANTERNでも

    THROW THE NERF BALL AT THE BRASS LANTERN

    THROW THE BRASS LANTERN AT THE NERF BALL

では前者の場合、LANTERNはPRSI、後者だとPRSOになるところです。


動詞の定義はSYNTAXで定義します。

```
<SYNTAX GET OBJECT = V-TAKE>
<SYNTAX GET IN OBJECT = V-ENTER>
<SYNTAX GET ON OBJECT = V-ENTER>
<SYNTAX GET OFF OBJECT = V-EXIT>
<SYNTAX GET OBJECT WITH OBJECT = V-TAKE-WITH>
```

OBJECTとなっているところは名詞で置き換えられます。

V-TAKEなどはROUTINE名です。
同じ動詞でもsyntaxによって振り分けることができます。

またSYNTAX内の括弧で囲まれた(HAVE)などのトークンは特別な意味を持ち、

```lisp
<SYNTAX GIVE OBJECT (HAVE)
     TO OBJECT (ON-GROUND IN-ROOM) = V-GIVE>
```

この例では、playerがOBJECTを持っていない場合、パース時にエラーとなります。

ACTIONやSYNTAXでいかに頑張るかが開発者の腕の見せ所のようです。
ZILは英語を前提として作られているため、日本語への移植などは大変だったのではないでしょうか。


## まとめ

他にもEventなどがありますが、ここでは省略します。 興味がある方はZIL Manualを読んで見てください。

ZILはIFを作る目的でデザインされたコンパクトな言語です。
あえて今テキストアドベンチャーを作るためにZILを使う必要はありませんが、IFの構成要素を知るためにZILを調べて見るのも面白いと思います。
