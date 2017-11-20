title: pypiのミラーを作る
date: 2017-11-13
tags: server, python

[bandersnatch](https://pypi.python.org/pypi/bandersnatch)を使用してpypiミラーを作成します。

数日ほど時間がかかる上、500GiBぐらいディスクを使います。

# bandersnatchのインストール

```sh
$ virtualenv bandersnatch
$ cd bandersnatch
$ bin/pip install -r https://bitbucket.org/pypa/bandersnatch/raw/stable/requirements.txt
```

# bandersnatch.conf

```conf
# ミラーのコピー先
directory = /usr/local/mirror/pypi/
master = https://pypi.python.org
timeout = 10
# worker=1だとすごく時間がかかる
workers = 3
hash-index = false
stop-on-error = false
delete-packages = true
```

# 不要なバイナリのコピーをやめる

完全なミラーを作る必要がなく、linuxでだけ使用するのでwin32, macosxのwhlなどは不要です。

lib/python3.6/site-packages/bandersnatch/package.py 
にパッチを当ててダウンロードしないようにしました。
バージョンは2.0.0です。

```python
    def sync_release_files(self):
        release_files = []

        for release in self.releases.values():
            release_files.extend(release)

        self.purge_files(release_files)

        for release_file in release_files:
            self.download_file(release_file['url'], release_file['md5_digest'])
```

ここにダウンロードしない条件を入れました

```python
    def sync_release_files(self):
        release_files = []

        for release in self.releases.values():
            release_files.extend(release)

        self.purge_files(release_files)

        for release_file in release_files:
           if re.search('macosx|win32|win_amd64', release_file['url']):
               logger.info(u'skip: {0}'.format(release_file['url']))
           else:
               self.download_file(release_file['url'], release_file['md5_digest'])

```

# 実行

かなり時間がかかります。

```sh
bandersnatch -c ./bandersnatch.conf mirror
```


# webサーバーを立てる

nginxをインストールし、ミラーしたパスへaliasを設定します。

nginx.conf
```conf

server {

    ...

    location /pypi {
        alias /usr/local/mirror/pypi/web;
        autoindex on;
        charset utf-8;
    }
}

```

# pip.confの設定

$HOME/.config/pip/pip.conf

```conf
index-url = http://hostname/pipy/simple
trusted-host = hostname
```

これで`pip install`がインターネットに接続できない環境でも使えるようになりました。

