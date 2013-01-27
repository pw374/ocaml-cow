(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2010-2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(* Atom Syndication format output. Bare minimum for a reader to use, feel
   free to extend from the full spec at:
   http://www.atomenabled.org/developers/syndication/atom-format-spec.php
*)
type author = {
  name : string;
  uri  : string option;
  email: string option;
}

let xml_of_author a =
  Xml.tag "author" [
    Xml.tag "name"  (Xml.string a.name);
    Xml.tag "uri"   (Xml.string a.uri);
    Xml.tag "email" (Xml.string a.email);
  ]

type date = {
  year : int;
  month: int;
  day  : int;
  hour : int;
  min  : int;
}

let xml_of_date d =
  let str = Printf.sprintf "%.4d-%.2d-%.2dT%.2d:%.2d:00Z" d.year d.month d.day d.hour d.min in
  Xml.string str

type meta = {
  id      : string;
  title   : string;
  subtitle: string option;
  author  : author option;
  rights  : string option;
  updated : date;
}

let xml_of_meta m =
  let option v f = match v with
    | None   -> []
    | Some v -> f v in
  let xml_of_subtitle s = Xml.tag "subtitle" (Xml.string s) in
  let xml_of_rights s = Xml.tag "rights" (Xml.string s) in
  Xml.tag "meta" (
    Xml.append [
      Xml.tag "id" (Xml.string m.id);
      Xml.tag "title" (Xml.string m.title);
    ]
    @ option m.subtitle xml_of_subtitle
    @ opton m.author xml_of_author
    @ option m.rights xml_of_rights
    @ [ xml_of_date d.date ]
  )

type content = Xml.t

let xml_of_content c = <:xml<
  <content type="xhtml">
   <div xmlns="http://www.w3.org/1999/xhtml">
     $c$
   </div>
  </content>
>>

type summary = string option

let xml_of_summary = function
  | None     -> []
  | Some str -> <:xml<<summary>$str:str$</summary>&>>

type entry = {
  entry: meta;
  summary: summary;
  content: content;
}

let xml_of_entry e = <:xml<
  <entry>
    $xml_of_meta e.entry$
    $xml_of_summary e.summary$
    $xml_of_content e.content$
  </entry>
>>

let contributors entries =
  List.fold_left
    (fun accu e -> match e.entry.author with
      | None   -> accu
      | Some a -> if List.mem a accu then accu else a::accu)
    []
    entries

let xml_of_contributor c =
  <:xml<<contributor>$xml_of_author c$</contributor>&>>

type feed = {
  feed: meta;
  entries: entry list;
}

let xml_of_feed ? self f =
  let self = match self with
    | None   -> []
    | Some s -> <:xml<<link rel="self" href=$str:s$/>&>> in
<:xml<
  <feed xmlns="http://www.w3.org/2005/Atom">
     $self$
     $xml_of_meta f.feed$
     $list:List.map xml_of_contributor (contributors f.entries)$
     $list:List.map xml_of_entry f.entries$
  </feed>
>>

let compare (yr1,mn1,da1,_,_) (yr2,mn2,da2,_,_) =
  match yr1 - yr2 with
    | 0 ->
      (match mn1 - mn2 with
        | 0 -> da1 - da2
        | n -> n
      )
    | n -> n