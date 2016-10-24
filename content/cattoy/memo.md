title: cattoy memo
date: 2016-10-24


* [https://www.cattoy.info](https://www.cattoy.info)
    * Cat Exercise Wheelの使ってくれなさのあまり作りました。
* さくらVPS mem1G/2Coreプラン
* CentOS 6.8
* nginx 
    * http2に対応するためビルドした
    * ついでにLibreSSLにした

```
nginx version: nginx/1.11.5
built by gcc 4.4.7 20120313 (Red Hat 4.4.7-17) (GCC) 
built with LibreSSL 2.5.0
TLS SNI support enabled
configure arguments: --prefix=/opt/nginx-1.11.5 --with-http_ssl_module --with-http_gzip_static_module --with-http_stub_status_module --with-http_v2_module --with-http_realip_module --with-openssl=../libressl-2.5.0 --with-cc-opt='-O2 -g -pipe -Wall -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -fstack-protector --param=ssp-buffer-size=4 -m64 -march=native' --with-ld-opt=' -Wl,-E -lrt'
```


* Let's Encryptで証明書とってhttp2化
    * 自配信コンテンツはほとんどないので意味はない


* ruby 2.3.1
* rails 5.0.1
    * puma
* sqlite3
    * DB動かすほどのサーバーじゃないのでとりあえず
* 認証はsorcery
* エラー通知はexception\_notification -> slack-notifierでcattoy.slack -> https://cattoy.slack.com/
* itamae
    * nginx -> pumaまわりが未完成
    * 面倒になってきたのでdockerの方が楽そうな気がしてきた
* CSSフレームワークは[kathamo](https://github.com/kathamo/Kathamo/tree/master/Kathamo-3.0.0)
    * navbarのopen/closeとturbolinksが相性が悪く、はまった。turbolinksは無効化
