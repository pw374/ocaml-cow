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
open Ast
%}

%token <string> STRING
%token OPEN CLOSE
%token EOF
%token SEMI

%type <Ast.declaration> declaration
%type <Ast.statement> statement
%type <Ast.rule> rule
%type <Ast.t> main

%start main

%%

declaration:
| STRING SEMI { Ast.declaration_of_string $1 }
;;

rule:
| STRING OPEN statements CLOSE { Ast.rule $1 $3 }
;;

statement:
| STRING SEMI {
    match Ast.simple_statement_of_string $1 with
    | `decl d -> Declaration d
    | `at s   -> Ast.at s
  }
| rule        { Rule $1 }
;;

statements:
| statement            { [$1] }
| statement statements { $1 :: $2 }
;;

main:
| statements { $1 }
;;

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
  | Lexing_error msg     as e -> error lexbuf e msg
  | Parsing.Parse_error as e -> error lexbuf e "parse error"
