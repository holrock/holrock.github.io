title: AllegroGraph その2
date: 2016-11-04
tag: lisp,allegrograph,ruby,cattoy

AllegroGraphが動くようになったので、データ作って投入します。

データは[cattoy](https://www.cattoy.info)から作ります。

[こんなかんじ](https://github.com/holrock/cattoy/commit/202415749077a2820bff11d3544af3fc03453b65)
にRDF出力コードを仕込んだので、

```ruby
g = RDF::Graph.new.tap {|g| Cat.all.each {|c| g << c.to_rdf }}
File.open('/tmp/cat.ttl', 'w') {|f| f << g.to_ttl }
```
でファイルに吐き出します。
なかみはこんなかんじです。

```
@prefix schema: <http://schema.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<http://www.cattoy.info/histories/1> schema:creator <http://www.cattoy.info/cats/1>;
   schema:itemReviewed <http://www.cattoy.info/toys/1>;
   schema:ratingValue 1;
   schema:reviewBody "羽よりネズミが好き。音がして嚙めるものが好きっぽい" .

<http://www.cattoy.info/histories/10> schema:creator <http://www.cattoy.info/cats/1>;
   schema:itemReviewed <http://www.cattoy.info/toys/10>;
   schema:ratingValue 1;
   schema:reviewBody "たまに猫キックしてた" .

<http://www.cattoy.info/cats/1> schema:name "みけ" .
```

これをWebViewに取り込んで、クエリを投げます

```
prefix sc: <http://schema.org/>

select ?cat ?history ?body {
  ?cat sc:name "みけ".
  ?history sc:creator ?cat;
  sc:reviewBody ?body;
  sc:ratingValue "-1"^^<http://www.w3.org/2001/XMLSchema#integer>.
}
```


結果は


```javascript
{
   "head" : {
      "vars" : [
         "cat",
         "history",
         "body"
      ]
   },
   "results" : {
      "bindings" : [
         {
            "history" : {
               "value" : "http://www.cattoy.info/histories/107",
               "type" : "uri"
            },
            "body" : {
               "value" : "これのピンク色のやつ\r\n音に期待したけど全然だめ。だめ",
               "type" : "literal"
            },
            "cat" : {
               "type" : "uri",
               "value" : "http://www.cattoy.info/cats/1"
            }
         },
         {
            "cat" : {
               "type" : "uri",
               "value" : "http://www.cattoy.info/cats/1"
            },
            "history" : {
               "type" : "uri",
               "value" : "http://www.cattoy.info/histories/2"
            },
            "body" : {
               "type" : "literal",
               "value" : "鉄のストラップの方に興味が行くほど、光線に興味なし。"
            }
         },
         // snip
```

無事取れてますが、これがないとダメでした。

```
  sc:ratingValue "-1"^^<http://www.w3.org/2001/XMLSchema#integer>.
```


しかし、述語選ぶのが大変ですね。
