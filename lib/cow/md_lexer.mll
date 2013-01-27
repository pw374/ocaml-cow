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

{
open Md_parser

let new_lines lexbuf =
  let str = Lexing.lexeme lexbuf in
  for i = 0 to String.length str - 1 do
    if str.[i] = '\n' then Lexing.new_line lexbuf;
  done
}

let char = [^ ' ' '\t' '\r' '[' ']' '(' ')' '\n']
let space = [' ' '\t' '\r']
let eol = ['\n']
let emptyline = eol+ space* eol+

(* very very very simple CSS lexer *)
rule token = parse
  | space+     { token lexbuf }
  | emptyline  { new_lines lexbuf; EMPTYLINE }
  | '\n'       { Lexing.new_line lexbuf; token lexbuf }
  | '['        { LBRACKET }
  | ']'        { RBRACKET }
  | '('        { LPAR }
  | ')'        { RPAR }
  | eof        { EOF }
  | char+ as x { new_lines lexbuf; STRING x }
