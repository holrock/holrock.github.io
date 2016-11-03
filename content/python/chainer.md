title: chainerで分類(できてない)
date: 2016-10-25
tag: chainer, python

cattoyでおもちゃの分類を自動でやりたくてchainerを試しました。
cattoyに登録されている画像を集めてきて手作業で分類、学習をやってみましたが、
やはり分類が面倒なのと、素材不足で全然だめでした。

やったことをメモしておきます。

* pip install chainer pillow 
* 画像の数が多い猫じゃらし、爪とぎ、その他で分類してみる。images/nekojarashi images/tsumetogi images/otherとフォルダごとに分類
* [http://ksksksks2.hatenadiary.jp/entry/20160327/1459061931](http://ksksksks2.hatenadiary.jp/entry/20160327/1459061931)
を参考にリサイズとトレーニングデータに分ける

```python
line = path + " " + label + "\n"
```

の所がlabelのままだとcompute\_mean.pyでエラーになるので

```python
line = path + " " + str(i) + "\n"
```

に変更する。

* python chainer/examples/imagenet/compute\_mean.py --root . train\_list.txt
* python chainer/examples/imagenet/train\_imagenet.py -g -1 --test --epoch 50 train\_list.txt validate\_list.txt
    * GPUないので-1。遅い

```
epoch       iteration   main/loss   validation/main/loss  main/accuracy  validation/main/accuracy  lr
5           10          6.24469     3.47694               0.378125       0.777778                  0.01
10          20          3.63611     2.24688               0.571875       0.777778                  0.01
15          30          1.78171     1.3616                0.471875       0.111111                  0.01
20          40          1.13622     0.96606               0.521875       0.777778                  0.01
26          50          0.987004    0.830154              0.553125       0.777778                  0.01
31          60          0.974401    1.08021               0.54375        0.111111                  0.01
36          70          0.966584    0.874548              0.528125       0.777778                  0.01
41          80          0.96336     0.798205              0.553125       0.777778                  0.01
47          90          0.956709    0.891943              0.546875       0.777778                  0.01
```

と、まあ微妙な感じでした
