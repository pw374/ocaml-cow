(*
  Copyright (c) 2009 Mauricio Fernández <mfp@acm.org>
  Copyright (c) 2009-2010 Anil Madhavapeddy <anil@recoil.org>
  Copyright (c) 2012 Guillem Rieu <guillem.rieu@ocamlpro.com>
  Copyright (c) 2010-2013 Thomas Gazagnaire <thomas@gazagnaire.org>

  Permission is hereby granted, free of charge, to any person
  obtaining a copy of this software and associated documentation
  files (the "Software"), to deal in the Software without
  restriction, including without limitation the rights to use,
  copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following
  conditions:

  The above copyright notice and this permission notice shall be
  included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
  OTHER DEALINGS IN THE SOFTWARE.
*)

type text =
  | String of string
  | Em of string
  | Bold of string
  | Del of string
  | Code of string
  | Link of Xhtml.link
  | Anchor of string
  | Image of Xhtml.image

and paragraph =
  | Paragraph of text list
  | Html of Xhtml.t
  | Pre of string * string option
  | Heading of int * text list
  | Quote of paragraph list
  | Ulist of paragraph list
  | Olist of paragraph list

type t = paragraph list

val to_html : t -> Xhtml.t

val to_string: t -> string

val of_string : string -> t

val of_file: string -> t

val assert_equal: t -> t -> unit

(** Create a heading *)
val id_of_heading: paragraph -> string

(*
(** Extract a HTML table of contents from markdown elements. Depth can
    be modified with the corresponding optional argument. *)
val to_html_toc:
  ?wrap_list:(depth:int -> XHtml.t -> Html.t) ->
  ?wrap_item:(depth:int -> heading:paragraph -> Html.t -> Html.t) ->
  ?depth:int -> t -> Html.t
*)
