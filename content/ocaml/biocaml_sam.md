title: Biocaml SAM header
date: 2017-02-21
tags: ocaml

ocaml製のバイオインフォマティクス用ライブラリで、
[biocaml](http://biocaml.org)というのがあります。

BAMファイルを読みたかったので使ってみましたが、
`@HG`ヘッダーにある`GO`(group order)に未対応で読めなかったため、
ちょっと修正しました。

opamで落ちてくる0.6.0用です。

```ocaml
utop # #require "biocaml.unix";;
utop # open Biocaml_unix.Std;;
utop # Bam.read (open_in "a.bam");;
- : (Bam.Header.t * Sam.alignment Core_kernel.Std.Or_error.t Stream.t)
Core_kernel.Result_wrapper.Result.Error
 ("unexpected tag for given header item type" (HD (GO)))
```

```diff
diff --git a/lib/unix/sam.ml b/lib/unix/sam.ml
index 9bd17ac..2da7b75 100644
--- a/lib/unix/sam.ml
+++ b/lib/unix/sam.ml
@@ -26,9 +26,13 @@ type tag_value = string * string
 type sort_order = [ `Unknown | `Unsorted | `Query_name | `Coordinate ]
 [@@deriving sexp]
 
+type group_order = [ `None | `Query | `Reference ]
+[@@deriving sexp]
+
 type header_line = {
   version : string;
   sort_order : sort_order option;
+  group_order: group_order option;
 } [@@deriving sexp]
 
 type ref_seq = {
@@ -81,6 +85,7 @@ type header_item = [
 type header = {
   version : string option;
   sort_order : sort_order option;
+  group_order : group_order option;
   ref_seqs : ref_seq list;
   read_groups : read_group list;
   programs : program list;
@@ -91,6 +96,7 @@ type header = {
 let empty_header = {
   version = None;
   sort_order = None;
+  group_order = None;
   ref_seqs = [];
   read_groups = [];
   programs = [];
@@ -192,9 +198,9 @@ let parse_header_version s =
     else
       err
 
-let header_line ~version ?sort_order () =
+let header_line ~version ?sort_order ?group_order () =
   parse_header_version version >>| fun version ->
-  {version; sort_order}
+  {version; sort_order; group_order}
 
 let ref_seq
     ~name ~length
@@ -269,7 +275,7 @@ let read_group
 
 
 let header
-    ?version ?sort_order ?(ref_seqs=[]) ?(read_groups=[])
+    ?version ?sort_order ?group_order ?(ref_seqs=[]) ?(read_groups=[])
     ?(programs=[]) ?(comments=[]) ?(others=[])
     ()
     =
@@ -302,7 +308,7 @@ let header
   |> List.filter_map ~f:Fn.id
   |> function
      | [] -> Ok {
-       version; sort_order; ref_seqs; read_groups;
+       version; sort_order; group_order; ref_seqs; read_groups;
        programs; comments; others;
      }
      | errs -> Error (Error.of_list errs)
@@ -407,12 +413,20 @@ let parse_sort_order = function
   | "coordinate" -> Ok `Coordinate
   | x -> error "invalid sort order" x sexp_of_string
 
+let parse_group_order = function
+  | "none" -> Ok `None
+  | "query" -> Ok `Query
+  | "reference" -> Ok `Reference
+  | x -> error "invalid group order" x sexp_of_string
+
 let parse_header_line tvl =
   find1 `HD tvl "VN" >>= fun version ->
   find01 `HD tvl "SO" >>?~
   parse_sort_order >>= fun sort_order ->
-  assert_tags `HD tvl ["VN"; "SO"] >>= fun () ->
-  header_line ~version ?sort_order ()
+  find01 `HD tvl "GO" >>?~
+  parse_group_order >>= fun group_order ->
+  assert_tags `HD tvl ["VN"; "SO"; "GO"] >>= fun () ->
+  header_line ~version ?sort_order ?group_order ()
 
 let parse_ref_seq tvl =
   find1 `SQ tvl "SN" >>= fun name ->
@@ -844,13 +858,25 @@ let print_sort_order x =
     | `Coordinate -> "coordinate"
     )
 
-let print_header_line ({version; sort_order} : header_line) =
-  sprintf "@HD\tVN:%s%s"
+let print_group_order x =
+  print_tag_value' "GO"
+    (match x with
+    | `None -> "none"
+    | `Query -> "query"
+    | `Reference -> "reference"
+    )
+
+let print_header_line ({version; sort_order; group_order} : header_line) =
+  sprintf "@HD\tVN:%s%s%s"
     version
     (match sort_order with
     | None -> ""
     | Some x -> sprintf "\t%s" (print_sort_order x)
     )
+    (match group_order with
+    | None -> ""
+    | Some x -> sprintf "\t%s" (print_group_order x)
+    )
 
 let print_ref_seq (x:ref_seq) =
   sprintf "@SQ\tSN:%s\tLN:%d%s%s%s%s"
@@ -1011,12 +1037,12 @@ module MakeIO(Future : Future.S) = struct
           Pipe.junk lines >>= fun () ->
           parse_header_item line |> function
           | Error _ as e -> return e
-          | Ok (`HD ({version; sort_order} : header_line)) -> (
+          | Ok (`HD ({version; sort_order; group_order} : header_line)) -> (
             match hdr.version with
             | Some _ ->
               return (Or_error.error_string "multiple @HD lines not allowed")
             | None ->
-              loop {hdr with version = Some version; sort_order}
+              loop {hdr with version = Some version; sort_order; group_order}
           )
           | Ok (`SQ x) -> loop {hdr with ref_seqs = x::hdr.ref_seqs}
           | Ok (`RG x) -> loop {hdr with read_groups = x::hdr.read_groups}
@@ -1028,14 +1054,14 @@ module MakeIO(Future : Future.S) = struct
     in
     loop empty_header >>| function
     | Error _ as e -> e
-    | Ok ({version; sort_order; _} as x) ->
+    | Ok ({version; sort_order; group_order; _} as x) ->
       let ref_seqs = List.rev x.ref_seqs in
       let read_groups = List.rev x.read_groups in
       let programs = List.rev x.programs in
       let comments = List.rev x.comments in
       let others = List.rev x.others in
       header
-        ?version ?sort_order ~ref_seqs ~read_groups
+        ?version ?sort_order ?group_order ~ref_seqs ~read_groups
         ~programs ~comments ~others ()
 
 
@@ -1064,7 +1090,7 @@ module MakeIO(Future : Future.S) = struct
     (match h.version with
     | None -> Deferred.unit
     | Some version ->
-      write_line w (print_header_line {version; sort_order=h.sort_order})
+      write_line w (print_header_line {version; sort_order=h.sort_order; group_order=h.group_order})
     ) >>= fun () ->
     Deferred.List.iter h.ref_seqs ~f:(fun x ->
       write_line w (print_ref_seq x)
diff --git a/lib/unix/sam.mli b/lib/unix/sam.mli
index 1fbf606..ff4da2a 100644
--- a/lib/unix/sam.mli
+++ b/lib/unix/sam.mli
@@ -28,6 +28,9 @@ type tag_value = private string * string
 type sort_order = [ `Unknown | `Unsorted | `Query_name | `Coordinate ]
 [@@deriving sexp]
 
+type group_order = [ `None | `Query | `Reference ]
+[@@deriving sexp]
+
 (** @HD. A header consists of different types of lines. Confusingly, one of
     these types is called {i the} "header line", which is what this
     type refers to. It does not refer generically to any line within a
@@ -35,6 +38,7 @@ type sort_order = [ `Unknown | `Unsorted | `Query_name | `Coordinate ]
 type header_line = private {
   version : string; (** VN *)
   sort_order : sort_order option; (** SO *)
+  group_order: group_order option; (** GO *)
 } [@@deriving sexp]
 
 (** @SQ. Reference sequence. *)
@@ -103,6 +107,7 @@ type header_item = private [<
 type header = private {
   version : string option;
   sort_order : sort_order option;
+  group_order : group_order option;
   ref_seqs : ref_seq list;
   read_groups : read_group list;
   programs : program list;
@@ -229,6 +234,7 @@ include module type of MakeIO(Future_unix)
 val header_line
   :  version:string
   -> ?sort_order:sort_order
+  -> ?group_order:group_order
   -> unit
   -> header_line Or_error.t
 
@@ -264,6 +270,7 @@ val read_group
 val header
    : ?version:string
   -> ?sort_order:sort_order
+  -> ?group_order:group_order
   -> ?ref_seqs : ref_seq list
   -> ?read_groups : read_group list
   -> ?programs : program list
```

```ocaml
utop # Bam.read (open_in "a.bam");;
- : (Bam.Header.t * Sam.alignment Core_kernel.Std.Or_error.t Stream.t)
Core_kernel.Std.Or_error.t                                                                                                                                                                  = Core_kernel.Result_wrapper.Result.Ok (<abstr>, <abstr>) 
```

masterブランチはビルド失敗するし、ドキュメントとも乖離が激しいので
積極的にメンテナンスはされていないようですね。

追記。プルリクエスト取り込んでもらいました。
