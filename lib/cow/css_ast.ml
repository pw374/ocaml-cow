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

(** Nested CSS *)

type declaration = {
  property: string;
  value: string;
}

exception Lexing_error of string

let lexing_error fmt =
  Printf.kprintf (fun str -> raise (Lexing_error str)) fmt

type statement =
  | Declaration of declaration
  | Rule of rule
  | At of string

and rule = {
  selector: string;
  body: statement list;
}

type t = statement list

let lstrip s =
  let c = ref 0 in
  while s.[!c] = ' ' && !c < String.length s do
    incr c
  done;
  if !c <> 0 then
    String.sub s !c (String.length s - !c)
  else
    s

let rstrip s =
  let c = ref (String.length s - 1) in
  while s.[!c] = ' ' && !c >= 0 do
    decr c
  done;
  if !c <> String.length s - 1 then
    String.sub s 0 (!c + 1)
  else
    s

let strip s =
  lstrip (rstrip s)


let rule selector body =
  let selector = strip selector in
  Rule { selector; body }

let at s =
  At (strip s)

let simple_statement_of_string str =
  try
    let i = String.rindex str ':' in
    let property = strip (String.sub str 0 i) in
    let value = strip (String.sub str (i+1) (String.length str - i - 1)) in
    `decl { property; value }
  with Not_found ->
    `at str

let declaration_of_string str =
  match simple_statement_of_string str with
  | `decl d -> d
  | _       -> lexing_error "lexing error: %S is not a valid declaration" str
