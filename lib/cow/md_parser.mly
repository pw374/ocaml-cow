/*
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
 */

%{
let link text href =
  let text = String.concat " " text in
  let href = String.concat " " href in
  { Xhtml.text; href }
%}

%token <string> STRING
%token RBRACKET LBRACKET
%token LPAR RPAR
%token EMPTYLINE
%token EOF
%token SEMI

%type <Md_ast.t> main

%start main

%%

link:
| LBRACKET string_list RBRACKET LPAR string_list RPAR
    { Md_ast.Link (link $2 $5) }
| LBRACKET string_list RBRACKET
    { Md_ast.Link (link $2 $2) }
;;

string_list:
| STRING             { [$1] }
| STRING string_list { $1 :: $2 }
;;

text:
| link   { $1 }
| STRING { Md_ast.text_of_string $1 }
;;

paragraph:
| EMPTYLINE      { [] }
| text paragraph { $1 :: $2 }
;;

paragraph_list:
| paragraph                { [Md_ast.Paragraph $1] }
| paragraph paragraph_list { (Md_ast.Paragraph $1) :: $2 }
;;

main:
| EOF                { [] }
| paragraph_list EOF { $1 }

%%

let error lexbuf exn msg =
  let curr = lexbuf.Lexing.lex_curr_p in
  let start = lexbuf.Lexing.lex_start_p in
  Printf.eprintf
    "File %S, line %d, character %d-%d:\n%s."
    curr.Lexing.pos_fname
    start.Lexing.pos_lnum
    (start.Lexing.pos_cnum - start.Lexing.pos_bol)
    (curr.Lexing.pos_cnum - curr.Lexing.pos_bol)
    msg;
  raise exn

let main token lexbuf =
  try main token lexbuf
  with
  | Parsing.Parse_error  as e -> error lexbuf e "parse error"