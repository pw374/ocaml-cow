(*
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

let text_of_string str =
  if String.length str > 2 then (
    let extract () = String.sub str 1 (String.length str - 2) in
    match str.[0], str.[String.length str - 1] with
    | '*', '*' -> Bold (extract ())
    | '_', '_' -> Em (extract ())
    | '=', '=' -> Del (extract ())
    | '`', '`' -> Code (extract ())
    | _        -> String str
  ) else
    String str