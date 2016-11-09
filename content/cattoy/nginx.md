title: nginxでimage filter
date: 2016-11-10
tags: cattoy,nginx

前回、画像プロキシを使って外部画像を読み込むようにしました。

画像サイズが大きいものもあり、PageSpeed Insightsに怒られることがあります。
今回は画像をキャッシュしつつリサイズします。

[参考](http://tech.actindi.net/3571355260)

すべて同一サーバーで運用しています。

# tlsimg.cattoy.info

画像フロントサーバー

httpsで待ち受けるサーバーです。ここが入り口になります。

キャッシュもここでします。

proxy\_cache\_keyに使用する$arg\_w, $arg\_h, arg\_qがリサイズパラメータになります。
それぞれ幅、高さ、jpeg qualityです。

バックエンドがomacなので、$document\_uriにhmacのキーを含んでいるため、サイズ指定以外は$document\_uri があればユニークになります。

```nginx
  server {
    server_name tlsimg.cattoy.info;
    location / {
      proxy_pass http://localhost:8889;
      proxy_cache TLSIMG;
      proxy_cache_key "$host$document_uri$is_args&$arg_w&$arg_h&$arg_q";
      proxy_cache_lock on;
      proxy_cache_valid 30d;
      proxy_cache_valid any 15s;
      proxy_cache_use_stale error timeout invalid_header updating;
      proxy_http_version 1.1;
      expires 30d;
    }
```

# localhost:8889

画像リサイズサーバー

クエリーをみて、リサイズします。
クエリーがない場合はそのまま画像プロキシへリクエストを流します。

画像プロキシ側にはurlパラメータを引き渡さないといけないので、ちょっと面倒です。

```nginx
  server {
    server_name _;
    listen 8889;
    location / {
      if ($arg_w = '') {
        rewrite ^/(.*)$ /original/$1 last;
      }
      set $width 240;
      set $height 240;
      set $quality 75;
      if ($arg_w ~ (\d*)) {
        set $width $1;
      }
      if ($arg_h ~ (\d*)) {
        set $height $1;
      }
      if ($arg_q ~ (100|[1-9][0-9]|[1-9])) {
        set $quality $1;
      }
      rewrite ^/(.*)$ /resize/$1 last;
    }
    location /resize/ {
      rewrite ^/resize/([^/]+) /$1?url=$arg_url break;
      proxy_pass http://omac/;
      internal;
      image_filter                resize $width $height;
      image_filter_jpeg_quality   $quality;
      image_filter_buffer         20M;
      image_filter_interlace      on;
      error_page 415 = @empty;
    }
    location /original/ {
      internal;
      proxy_pass http://omac/;
    }
    location @empty {
      empty_gif;
    }
  }
```


# 画像プロキシ

最後に画像プロキシ側の設定です。
ここは特に変わっていません。

```
  upstream omac {
      server 127.0.0.1:8999;
  }
```

# 議論

もっと簡単な方法はありそうです。

本来の画像URLにw,h,qクエリパラメータがある場合はおかしくなりそうなので、
別途ヘッダで渡すなどした方が良いと思います。
今回はrails側のコードをなるべく変更せずに使用するため、クエリパラメーターに含めました。

リサイズ時にエラーになってしまうとempty\_gifがキャッシュされてしまい、以後画像が見えなくなるのでこれも要対応です。

