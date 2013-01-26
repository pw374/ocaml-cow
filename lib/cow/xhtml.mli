(*
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** XHTML library *)

(** HTML base type *)
type t

(** HTML links *)
type link = {
  text: string;
  href: string;
}

(** Convert to string *)
val to_string : t -> string

(** The type for character encodings. For `UTF_16, endianness is determined from the BOM. *)
type encoding = [ `ISO_8859_1 | `US_ASCII | `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]

(** Read from string *)
val of_string: ?encoding:encoding -> string -> t

(** {2 Combinators} *)

(** Build a tag *)
val tag: string -> ?attributes:(string * string) list -> t -> t

(** Convert a link to HTML *)
val link: link -> t

(** Convert a string to HTML *)
val string: string -> t

(** Convert an integer to HTML *)
val int: int -> t

(** Convert a float to HTML *)
val float: float -> t

(** Append a list of HTML nodes together *)
val append: t list -> t

(** The empty HTML node *)
val empty: t

(** HTML lists *)
module List: sig

  val ul: t -> t
  val li: t -> t
  val ol: t -> t
  val dl: t -> t
  val dt: t -> t
  val dd: t -> t

  (** Build unordered HTML lists *)
  val unordered: t list -> t

  (** Build ordered HTML lists *)
  val ordered: t list -> t

  (** Build definition HTML lists *)
  val definition: (t * t) list -> t

end

module Table: sig

  val th: t -> t
  val tr: t -> t
  val td: t -> t

  val thead: t -> t
  val tbody: t -> t
  val tfoot: t -> t

  (** Build an HTML table from a table of HTML elements *)
  val table: ?border:bool -> t array array -> t

end
