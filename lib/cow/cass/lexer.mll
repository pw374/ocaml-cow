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

{ open Parser }

let char = [^ ' ' '\t' '\r' '{' '}' ';' '\n' '/']
let space = [' ' '\t' '\r']
let eol = ['\n']
let value = char (char | space | eol)*

(* very very very simple CSS lexer *)
rule token = parse
  | space+     { token lexbuf }
  | "/*"       { comments lexbuf; token lexbuf }
  | '\n'       { Lexing.new_line lexbuf; token lexbuf }
  | '{'        { OPEN  }
  | '}'        { CLOSE }
  | ';'        { SEMI }
  | eof        { EOF }
  | value as x {
      for i = 0 to String.length x - 1 do
        if x.[i] = '\n' then Lexing.new_line lexbuf;
      done;
      STRING x }

and comments = parse
  | "*/" { () }
  | '\n' { Lexing.new_line lexbuf; comments lexbuf }
  | _    { comments lexbuf }
