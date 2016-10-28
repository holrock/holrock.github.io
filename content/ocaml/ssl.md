title: conduit with homebrew openssl
date: 2016-10-28

# 現象

cohttpでhttpsから`Client.get`すると
`(Failure "No SSL or TLS support compiled into Conduit")`
とエラーになった

# 対策

```
CPPFLAGS="-I$HOME/homebrew/opt/openssl/include" opam install ssl
```

# 原因

homebrewでopensslを入れており、homebrewのpathを/usr/localから$HOME/homebrewに変えていたため、
opamでsslをビルドできなかった。
