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

open Css_ast
type t = Css_ast.t

let of_string str =
  let lexbuf = Lexing.from_string str in
  Css_parser.main Css_lexer.token lexbuf

let of_file file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file };
  try
    let r = Css_parser.main Css_lexer.token lexbuf in
    close_in ic;
    r
  with e ->
    close_in ic;
    raise e

let to_string t =
  let buf = Buffer.create 1024 in
  let tab n = String.make (2*n) ' ' in
  let declaration n d =
    Printf.bprintf buf "%s%s: %s;\n" (tab n) d.property d.value in
  let rec statement n = function
    | Declaration d -> declaration n d
    | Rule r        -> rule n r
    | At s          -> Printf.bprintf buf "%s%s;\n" (tab n) s
  and rule n r =
    Printf.bprintf buf "%s%s {\n" (tab n) r.selector;
    List.iter (statement (n+1)) r.body;
    Printf.bprintf buf "%s}\n" (tab n) in
  List.iter (statement 0) t;
  Buffer.contents buf

let assert_equal t1 t2 =
  let string s1 s2 =
    if s1 <> s2 then (
      Printf.eprintf "%s <> %s!\n" s1 s2;
      exit 1
    ) in
  let declaration d1 d2 =
    string d1.property d2.property;
    string d1.value d2.value in
  let pp = function
    | Declaration _ -> "decl"
    | Rule _        -> "rule"
    | At _          -> "at" in
  let rec statement s1 s2 =
    match s1, s2 with
    | Declaration d1, Declaration d2 -> declaration d1 d2
    | Rule r1       , Rule r2        -> rule r1 r2
    | At s1         , At s2          -> string s1 s2
    | _ -> string (pp s1) (pp s2)
  and rule r1 r2 =
    string r1.selector r2.selector;
    List.iter2 statement r1.body r2.body in
  List.iter2 statement t1 t2
