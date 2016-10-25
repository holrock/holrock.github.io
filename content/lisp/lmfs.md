title: lmfsを見る
date: 2016-10-25

lsmfとはMTIのLispマシンのファイルシステムです。
unlambda.comからCADRのイメージとソースが落とせるので中身を見てみます。

# ダンプしてみる

* http://unlambda.com/index.php?n=Main.Cadr からファイルを落とす
    * CADR emulator tar file
    * system 78.48 source file partition

* usim.tar.gzを展開してlmfs.cをコンパイルする
    * コンパイルエラーと実行時のオーバランを修正

```diff
--- a/lmfs.c	2006-07-11 07:01:32.000000000 +0900
+++ b/lmfs.c	2016-09-18 22:24:12.000000000 +0900
@@ -127,6 +127,7 @@
 typedef int address;
 typedef int date;
 typedef char flag;
+typedef unsigned char u_char;
 
 typedef struct tapeinfo_s {
   date date;
@@ -467,7 +468,7 @@
 
   n = 0;
 
-  while (1) {
+  while (n < 64) {
     int left, use;
 
     left = remaining_access(&b);
```

* カレントディレクトリで`mkdir tmp`
* `./lmfs -f FILE.78.48`と動かすと、tmp内にファイルをダンプしてくれる
* FILE.sys99の方はsegmentation faultしてしまう
* CADR disk image(disk.img)はdiskmakerで結合しているので、そのままでは正当なパーティーションではない

```
00000000  4c 41 42 4c 01 00 00 00  2f 03 00 00 13 00 00 00  |LABL..../.......|
00000010  11 00 00 00 43 01 00 00  4d 43 52 31 4c 4f 44 31  |....C...MCR1LOD1|
00000020  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
00000040  43 41 44 52 20 64 69 73  6b 6d 61 6b 65 72 20 69  |CADR diskmaker i|
00000050  6d 61 67 65 00 20 20 20  20 20 20 20 20 20 20 20  |mage.           |
00000060  75 63 61 64 72 2e 6d 63  72 2e 38 34 31 00 00 00  |ucadr.mcr.841...|
00000070  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
```

# パーティションの構成

## partition-label

上記、unlambda.comで配布されているunix tar of source tree for system 78.48のtree/lmfs/fs-dcls.lispに定義がある

```lisp
(defstorage (partition-label)
  (version	fixnum)				;of structure
  (name						    char-with-length 30.)
  (label-size					    		     fixnum)				;in words
  (partition-id							     					fixnum)
  (addresses being disk-address
    primary-root				;for base partition
    free-store-info
    bad-track-info
    volume-table
  aspare1 aspare2 aspare3 aspare4)
  (update-times being date
    label-accepted				;put in use
    shut-down
    free-map-reconstructed
    structure-salvaged
    scavenged						;searching reclaimer
    tspare1 tspare2 tspare3 tspare4)
  
  (uid-generator	fixnum)
  (monotone-generator	fixnum)
  (root-list			fh-element)
  )
```

* partition-labelはpartitionの先頭に格納されている。固定長。
* CADRは32bitマシンなのでfixnumは`int32_t`
* `char-with-length 30.`で定義されているnameはパーティションラベル内に格納されると次のようになる
    * int16\_tで文字列長(fileなら4)
    * char[30]で文字列
    * lmfs.cだとchar[30]で読んでるが、`\０`を含まない30文字の模様
* 後はprimary-rootをたどってfile-header -> fh-element -> dir-header -> directory-entryと読むことになる(root directory)

* FILE.78.48 imageのpartition-labelを見ると、primary-rootが7139
* これはレコード番号なので、ブロックのサイズ(1024)とRECORD\_SIZE\_BLOCKS(4)をかけてオフセットを計算する
    * `7139 * 4 * 1024 = 29241344`

* 29241344のアドレスから読めばfile-headerが読めるはず
    * だが謎の8byteが存在するため、file-headerは29241344+8の29241352からになっている

* またブロックサイズは1024byteっぽいが1008byteで区切られている(lmfs.cのensure\_access関数)
    * 1024byte目にアクセスすると1008byteの位置へ調整される
    * 各ブロックごとに16byteのパディングがある模様
    * 次のようにアクセスする場所を調整している

```
0    1023 :    0 1007
1024 2047 : 1008 2015
2048 3071 : 2016 3025
3072 4095 : 3026 4033
```

* この辺は調べきれず


# file-header

```lisp
(defstorage (file-header)
  (version    fixnum)
  (logical-size   fixnum-bytes 2)
  (bootload-generation  fixnum)       ;for validating
  (version-in-bootload  fixnum)       ;used in above

  (number-of-elements fixnum-bytes 2)     ;for later expansion
  (ignore mod word)

  (parts being fh-element       ;and/or reformatting
    fh-header           ;this header itself
    info            ;header-resident info
    header-fm           ;file map of header
    dire            ;directory-entry-ifno
    property-list
    file-map
    pad-area            ;many natures of obsolescence
    pad2-area))           ;are supported
```

* logical-sizseは`(logical-size   fixnum-bytes 2)`となっているが、dump見ると4byteあるように見える

* partsの所が各fh-elementのエントリーになる
    * directory-entryはdireが差す場所になる

* コメントにもあるが、fh-headerはfile-header自身を指すエントリー。ちょっとわかりづらい

## fh-element

```lisp

(defstorage (fh-element)
  (name     char 4)
  (location   fixnum-bytes 2)     ;in words in log. headr
  (length   fixnum-bytes 2))      ;in words
```

* fh-elementのlocationとlengthはwordサイズなのでbyteサイズにするには\*4する
    * たとえはdirectory-entryは、29241352 + (0x1b\*4) から (0x23\*4)まで
    * \*DIR\*がname


```
% hexdump -C -s 29241460 -n 140 ./FILE.78.48
01be3074  04 00 72 6f 6f 74 00 00  00 00 00 00 00 00 00 00  |..root..........|
01be3084  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
01be3094  05 00 2a 44 49 52 2a 00  00 00 00 00 00 00 00 00  |..*DIR*.........|
01be30a4  01 00 00 08 00 00 00 00  00 00 00 00 00 00 00 00  |................|
01be30b4  00 00 00 00 04 00 00 00  01 00 00 00 de 79 5e c8  |.............y^.|
01be30c4  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
01be30d4  de 79 5e c8 00 00 00 00  00 00 00 00 e3 1b 00 00  |.y^.............|
01be30e4  00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|
01be30f4  00 00 00 00 00 00 00 00  04 00 00 00              |............|
01be3100
```

# dir-header

```lisp
(defstorage (dir-header)
  (version        fixnum-bytes 2)     ;structure version
  (size         fixnum-bytes 2)
  (name         char-with-length 30.)

  (numbers being (fixnum-bytes 2)

  number-of-entries          ;for scanning
  free-entry-list         ;for easier alloc
  entries-index-offset       ;to find first one
  direntry-size         ;for scanning
  entries-per-block          ;==

  default-generation-retention-count
  uid-path-offset
  uid-path-length
  hierarchy-depth)

  (default-volid      fixnum)       ;for creation

  (auto-expunge-p     flag)
  (auto-expunge-interval   date)
  (auto-expunge-last-time  date)

  (default-link-transparencies link-transparencies))

```

* dir-headerの直後に、number-of-entries * directory-entryのサイズだけdirectory-entryが格納されている

# directory-entry

* 大きいので省略

```lisp
(defstorage (directory-entry :abbrev dire)
  (file-name          char-with-length 30.)		;File name, to 30 chars
  (file-type	            char-with-length 14.)	      ;Extension
  (file-version		          fixnum-bytes 3.)			;Generation
  ...
  (record-0-address   disk-address)     ;how to find the file
```

* file-name, file-typeなどにファイルの情報
* record-0-addressがパーティションに格納されているレコードのアドレスになるので、そこからまたfile-headerを読んでいくことになる


この辺で力尽きました。
8byteや1008byte境界のあたりがわからず。
[aimemo528_75pgs.pdf](http://www.unlambda.com/cadr/aimemo528_75pgs.pdf)のDisk structureあたりの制約なのかなあ。

