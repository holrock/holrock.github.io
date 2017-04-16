title: OCaml 4.04.1
date: 2017-04-16
tags: ocaml


OCaml 4.04.1が出ました。
[Changes](https://caml.inria.fr/pub/distrib/ocaml-4.04/notes/Changes)


[Set.mapのバグ](https://github.com/ocaml/ocaml/pull/894)も修正されているので
インストールしようと思いましたが、`opam update`しても`opam switch list`に出てきません。


redditでも[話題](https://www.reddit.com/r/ocaml/comments/65bnx5/ocaml_4041_released_dedicated_to_the_memory_of/dg9bach/)
になっています。
[merge](https://github.com/ocaml/opam-repository/pull/8968)したから`opam update`した？というコメントもありますが、
実際のところ、`opam update`が参照しているhttps://opam.ocaml.org/urls.txtに変更が書かれていないため、リストに出てこないようです。


そうこうしているうちにhomebrewの方に4.04.1が来てたので`opam switch system`で済ませてしまいました。
