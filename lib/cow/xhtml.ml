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

open Xhtml_ast

type t = Xhtml_ast.t

type image = Xhtml_ast.image =  {
  src: string;
  alt: string;
}

type link = Xhtml_ast.link = {
  text: string;
  href: string;
}

let id x = x

let rec output_t o = function
  | (`Data _ as d) :: t ->
    Xmlm.output o d;
    output_t o t
  | (`El _ as e) :: t   ->
    Xmlm.output_tree id o e;
    Xmlm.output o (`Dtd None);
    output_t o t
  | [] -> ()

let to_string t =
  let buf = Buffer.create 1024 in
  let o = Xmlm.make_output (`Buffer buf) in
  (* XXX: check that this is working *)
  Xmlm.output o (`Dtd (Some "HTML"));
  output_t o t;
  Buffer.contents buf

(* XHTML 1.1 character entities.
   Transformed from the following data source:
   http://www.w3.org/\
   TR/xhtml-modularization/dtd_module_defs.html#a_xhtml_character_entities *)

let entities = [
  ("nbsp", "\194\160");
  ("iexcl", "\194\161");
  ("cent", "\194\162");
  ("pound", "\194\163");
  ("curren", "\194\164");
  ("yen", "\194\165");
  ("brvbar", "\194\166");
  ("sect", "\194\167");
  ("uml", "\194\168");
  ("copy", "\194\169");
  ("ordf", "\194\170");
  ("laquo", "\194\171");
  ("not", "\194\172");
  ("shy", "\194\173");
  ("reg", "\194\174");
  ("macr", "\194\175");
  ("deg", "\194\176");
  ("plusmn", "\194\177");
  ("sup2", "\194\178");
  ("sup3", "\194\179");
  ("acute", "\194\180");
  ("micro", "\194\181");
  ("para", "\194\182");
  ("middot", "\194\183");
  ("cedil", "\194\184");
  ("sup1", "\194\185");
  ("ordm", "\194\186");
  ("raquo", "\194\187");
  ("frac14", "\194\188");
  ("frac12", "\194\189");
  ("frac34", "\194\190");
  ("iquest", "\194\191");
  ("Agrave", "\195\128");
  ("Aacute", "\195\129");
  ("Acirc", "\195\130");
  ("Atilde", "\195\131");
  ("Auml", "\195\132");
  ("Aring", "\195\133");
  ("AElig", "\195\134");
  ("Ccedil", "\195\135");
  ("Egrave", "\195\136");
  ("Eacute", "\195\137");
  ("Ecirc", "\195\138");
  ("Euml", "\195\139");
  ("Igrave", "\195\140");
  ("Iacute", "\195\141");
  ("Icirc", "\195\142");
  ("Iuml", "\195\143");
  ("ETH", "\195\144");
  ("Ntilde", "\195\145");
  ("Ograve", "\195\146");
  ("Oacute", "\195\147");
  ("Ocirc", "\195\148");
  ("Otilde", "\195\149");
  ("Ouml", "\195\150");
  ("times", "\195\151");
  ("Oslash", "\195\152");
  ("Ugrave", "\195\153");
  ("Uacute", "\195\154");
  ("Ucirc", "\195\155");
  ("Uuml", "\195\156");
  ("Yacute", "\195\157");
  ("THORN", "\195\158");
  ("szlig", "\195\159");
  ("agrave", "\195\160");
  ("aacute", "\195\161");
  ("acirc", "\195\162");
  ("atilde", "\195\163");
  ("auml", "\195\164");
  ("aring", "\195\165");
  ("aelig", "\195\166");
  ("ccedil", "\195\167");
  ("egrave", "\195\168");
  ("eacute", "\195\169");
  ("ecirc", "\195\170");
  ("euml", "\195\171");
  ("igrave", "\195\172");
  ("iacute", "\195\173");
  ("icirc", "\195\174");
  ("iuml", "\195\175");
  ("eth", "\195\176");
  ("ntilde", "\195\177");
  ("ograve", "\195\178");
  ("oacute", "\195\179");
  ("ocirc", "\195\180");
  ("otilde", "\195\181");
  ("ouml", "\195\182");
  ("divide", "\195\183");
  ("oslash", "\195\184");
  ("ugrave", "\195\185");
  ("uacute", "\195\186");
  ("ucirc", "\195\187");
  ("uuml", "\195\188");
  ("yacute", "\195\189");
  ("thorn", "\195\190");
  ("yuml", "\195\191");
  ("lt", "<");
  ("gt", ">");
  ("amp", "&");
  ("apos", "'");
  ("quot", "\"");
  ("OElig", "\197\146");
  ("oelig", "\197\147");
  ("Scaron", "\197\160");
  ("scaron", "\197\161");
  ("Yuml", "\197\184");
  ("circ", "\203\134");
  ("tilde", "\203\156");
  ("ensp", "\226\128\130");
  ("emsp", "\226\128\131");
  ("thinsp", "\226\128\137");
  ("zwnj", "\226\128\140");
  ("zwj", "\226\128\141");
  ("lrm", "\226\128\142");
  ("rlm", "\226\128\143");
  ("ndash", "\226\128\147");
  ("mdash", "\226\128\148");
  ("lsquo", "\226\128\152");
  ("rsquo", "\226\128\153");
  ("sbquo", "\226\128\154");
  ("ldquo", "\226\128\156");
  ("rdquo", "\226\128\157");
  ("bdquo", "\226\128\158");
  ("dagger", "\226\128\160");
  ("Dagger", "\226\128\161");
  ("permil", "\226\128\176");
  ("lsaquo", "\226\128\185");
  ("rsaquo", "\226\128\186");
  ("euro", "\226\130\172");
  ("fnof", "\198\146");
  ("Alpha", "\206\145");
  ("Beta", "\206\146");
  ("Gamma", "\206\147");
  ("Delta", "\206\148");
  ("Epsilon", "\206\149");
  ("Zeta", "\206\150");
  ("Eta", "\206\151");
  ("Theta", "\206\152");
  ("Iota", "\206\153");
  ("Kappa", "\206\154");
  ("Lambda", "\206\155");
  ("Mu", "\206\156");
  ("Nu", "\206\157");
  ("Xi", "\206\158");
  ("Omicron", "\206\159");
  ("Pi", "\206\160");
  ("Rho", "\206\161");
  ("Sigma", "\206\163");
  ("Tau", "\206\164");
  ("Upsilon", "\206\165");
  ("Phi", "\206\166");
  ("Chi", "\206\167");
  ("Psi", "\206\168");
  ("Omega", "\206\169");
  ("alpha", "\206\177");
  ("beta", "\206\178");
  ("gamma", "\206\179");
  ("delta", "\206\180");
  ("epsilon", "\206\181");
  ("zeta", "\206\182");
  ("eta", "\206\183");
  ("theta", "\206\184");
  ("iota", "\206\185");
  ("kappa", "\206\186");
  ("lambda", "\206\187");
  ("mu", "\206\188");
  ("nu", "\206\189");
  ("xi", "\206\190");
  ("omicron", "\206\191");
  ("pi", "\207\128");
  ("rho", "\207\129");
  ("sigmaf", "\207\130");
  ("sigma", "\207\131");
  ("tau", "\207\132");
  ("upsilon", "\207\133");
  ("phi", "\207\134");
  ("chi", "\207\135");
  ("psi", "\207\136");
  ("omega", "\207\137");
  ("thetasym", "\207\145");
  ("upsih", "\207\146");
  ("piv", "\207\150");
  ("bull", "\226\128\162");
  ("hellip", "\226\128\166");
  ("prime", "\226\128\178");
  ("Prime", "\226\128\179");
  ("oline", "\226\128\190");
  ("frasl", "\226\129\132");
  ("weierp", "\226\132\152");
  ("image", "\226\132\145");
  ("real", "\226\132\156");
  ("trade", "\226\132\162");
  ("alefsym", "\226\132\181");
  ("larr", "\226\134\144");
  ("uarr", "\226\134\145");
  ("rarr", "\226\134\146");
  ("darr", "\226\134\147");
  ("harr", "\226\134\148");
  ("crarr", "\226\134\181");
  ("lArr", "\226\135\144");
  ("uArr", "\226\135\145");
  ("rArr", "\226\135\146");
  ("dArr", "\226\135\147");
  ("hArr", "\226\135\148");
  ("forall", "\226\136\128");
  ("part", "\226\136\130");
  ("exist", "\226\136\131");
  ("empty", "\226\136\133");
  ("nabla", "\226\136\135");
  ("isin", "\226\136\136");
  ("notin", "\226\136\137");
  ("ni", "\226\136\139");
  ("prod", "\226\136\143");
  ("sum", "\226\136\145");
  ("minus", "\226\136\146");
  ("lowast", "\226\136\151");
  ("radic", "\226\136\154");
  ("prop", "\226\136\157");
  ("infin", "\226\136\158");
  ("ang", "\226\136\160");
  ("and", "\226\136\167");
  ("or", "\226\136\168");
  ("cap", "\226\136\169");
  ("cup", "\226\136\170");
  ("int", "\226\136\171");
  ("there4", "\226\136\180");
  ("sim", "\226\136\188");
  ("cong", "\226\137\133");
  ("asymp", "\226\137\136");
  ("ne", "\226\137\160");
  ("equiv", "\226\137\161");
  ("le", "\226\137\164");
  ("ge", "\226\137\165");
  ("sub", "\226\138\130");
  ("sup", "\226\138\131");
  ("nsub", "\226\138\132");
  ("sube", "\226\138\134");
  ("supe", "\226\138\135");
  ("oplus", "\226\138\149");
  ("otimes", "\226\138\151");
  ("perp", "\226\138\165");
  ("sdot", "\226\139\133");
  ("lceil", "\226\140\136");
  ("rceil", "\226\140\137");
  ("lfloor", "\226\140\138");
  ("rfloor", "\226\140\139");
  ("lang", "\226\140\169");
  ("rang", "\226\140\170");
  ("loz", "\226\151\138");
  ("spades", "\226\153\160");
  ("clubs", "\226\153\163");
  ("hearts", "\226\153\165");
  ("diams", "\226\153\166"); ]

let entity str =
  if List.mem_assoc str entities then
    Some (List.assoc str entities)
  else
    None

(*** XHTML parsing (using Xml) ***)
let input_tree input =
  let el (name, attrs) body : t = [ `El ((name, attrs), List.flatten body) ] in
  let data s = [`Data s] in
  Xmlm.input_tree ~el ~data input

type encoding = [ `ISO_8859_1 | `US_ASCII | `UTF_16 | `UTF_16BE | `UTF_16LE | `UTF_8 ]

let of_string ?encoding str =
  (* It is illegal to write <:html<<b>foo</b>>> so we use a small trick and write
     <:html<<b>foo</b>&>> *)
  let str =
    if str.[String.length str - 1] = '&' then
      String.sub str 0 (String.length str - 1)
    else
      str in
  (* input needs a root tag *)
  let str = Printf.sprintf "<xxx>%s</xxx>" str in
  try
    let i = Xmlm.make_input ~enc:encoding ~entity (`String (0,str)) in
    (* make_input builds a well-formed document, so discard the Dtd *)
    (match Xmlm.peek i with
    | `Dtd _ -> ignore (Xmlm.input i)
    | _      -> ());
    (* Remove the dummy root tag *)
    match input_tree i with
    | [ `El ((("","xxx"), []), body) ]-> body
    | _ -> raise Parsing.Parse_error
  with Xmlm.Error (pos, e) ->
    Printf.eprintf "%s\nParsing error: line %d, characters %d-%d:\b%s\n" str (fst pos) (snd pos) (snd pos) (Xmlm.error_message e);
    raise Parsing.Parse_error

let tag t ?(attributes=[]) xhtml: t =
  let name s = ("", s) in
  let attr (k,v) = name k, v in
  [ `El ((name t, List.map attr attributes), xhtml) ]

let string s = [`Data s]

let empty = []

let link l =
  let attributes = [
    ("href", l.href);
  ] in
  tag "a" ~attributes (string l.text)

let image i =
  let attributes = [
    ("src", i.src);
    ("alt", i.alt);
  ] in
  tag "img" ~attributes empty

let int i = of_string (string_of_int i)

let float f = of_string (string_of_float f)

let append l = List.flatten l

let i t = tag "i" t

let b t = tag "b" t

let em t = tag "em" t

let del t = tag "del" t

let code t = tag "code" t

let pre t = tag "pre" t

let blockquote t = tag "blockquote" t

let p t = tag "p" t

let nbsp = string "&nbsp;"

let mk_class = function
  | [] -> []
  | cs -> [ ("class", String.concat " " cs) ]

let div classes t =
  let attributes = mk_class classes in
  tag "div" ~attributes t

let span classes t =
  let attributes = mk_class classes in
  tag "span" ~attributes t

let with_class classes (t:t) =
  match classes with
  | [] -> t
  | _  ->
    match t with
    | [`El ((name, attributes), body)] ->
      let attributes = List.filter (fun ((k,_),_) -> k<>"class") attributes in
      let attributes = (("","class"), String.concat " " classes) :: attributes in
      [`El ((name, attributes), body)]
    | _ -> failwith "with_class: not a valid tag!"

let anchor name =
  let attributes = [
    ("name", name);
  ] in
  tag "a" ~attributes nbsp

let h1 t =
  tag "h1" t

let h2 t =
  tag "h2" t

let h3 t =
  tag "h3" t

let h4 t =
  tag "h4" t

module Table = struct

  let th t = tag "th" t

  let tr t = tag "tr" t

  let td t = tag "td" t

  let caption t = tag "caption" t

  let colgroup t = tag "colgroupt" t

  let col t = tag "col" t

  let thead t = tag "thead" t

  let tbody t = tag "tbody" t

  let tfoot t = tag "tfoot" t

  let table ?(border=false) t =
    let attributes = match border with
      | true  -> ["border", "1"]
      | false -> [] in
    let row a = tr (append (List.map td (Array.to_list a))) in
    let all = List.map row (Array.to_list t) in
    tag "table" ~attributes (append all)

end

module List = struct

  let ul t = tag "ul" t

  let li t = tag "li" t

  let ol t = tag "ol" t

  let dl t = tag "dl" t

  let dt t = tag "dt" t

  let dd t = tag "dd" t

  let unordered t =
    ul (append (List.map li t))

  let ordered t =
    ol (append (List.map li t))

  let definition t =
    dl (append (List.map (fun (h,b) -> (dt h @ dd b)) t))

end

