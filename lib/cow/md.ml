(*
  Copyright (c) 2009 Mauricio Fernández <mfp@acm.org>
  Copyright (c) 2009-2010 Anil Madhavapeddy <anil@recoil.org>
  Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.org>

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

type paragraph =
  | Normal of text list
  | Html of Xhtml.t
  | Pre of string * string option
  | Heading of int * text list
  | Quote of paragraph list
  | Ulist of paragraph list
  | Olist of paragraph list

and text =
  | Text of string
  | Emph of string
  | Bold of string
  | Del of text list
  | Code of string
  | Link of Xhtml.link
  | Anchor of string
  | Image of Xhtml.image

type t = paragraph list

let of_string _ =
  failwith "TODO"

let of_file _ =
  failwith "TODO"

(* Create a suitable ID given a Header element *)
let id_of_heading (h: paragraph): string =
  let rec text_list tl =
    let replace s = Re_str.global_replace (Re_str.regexp "[ ]+") "" s in
    String.concat "-" (List.map (fun t -> replace (text t)) tl)
  and text = function
    | Text s | Emph s | Bold s | Code s | Anchor s -> s
    | Del tl    -> text_list tl
    | Link link -> link.Xhtml.text
    | Image img -> img.Xhtml.alt in
  match h with
  | Heading (n,tl) ->
    let str = text_list tl in
    Printf.sprintf "h%d-%s" n str
  | _ -> failwith "id_of_heading: input element is not a heading!"

let rec text = function
  | Text t   -> Xhtml.string t
  | Emph t   -> Xhtml.i (Xhtml.string t)
  | Bold t   -> Xhtml.b (Xhtml.string t)
  | Del tl   -> Xhtml.del (text_list tl)
  | Code t   -> Xhtml.code (Xhtml.string t)
  | Link l   -> Xhtml.link l
  | Anchor a -> Xhtml.anchor a
  | Image i  -> Xhtml.image i

and text_list tl =
  Xhtml.append (List.map text tl)

let rec paragraph p =
  let heading_content h tl =
    Xhtml.append [
      Bootstrap.anchor (id_of_heading h);
      text_list tl;
    ] in
  match p with
  | Normal tl       -> text_list tl
  | Html html       -> Xhtml.p html
  | Pre (t,_)       -> Xhtml.pre (Xhtml.string t)
  | Heading (1,tl)  -> Xhtml.h1 (heading_content p tl)
  | Heading (2,tl)  -> Xhtml.h2 (heading_content p tl)
  | Heading (3,tl)  -> Xhtml.h3 (heading_content p tl)
  | Heading (_,tl)  -> Xhtml.h4 (heading_content p tl)
  | Quote pl        -> Xhtml.blockquote (paragraph_list pl)
  | Ulist pl        -> Xhtml.List.unordered (List.map paragraph pl)
  | Olist pl        -> Xhtml.List.ordered (List.map paragraph pl)

and paragraph_list pl =
  let aux p = Xhtml.p (paragraph p) in
  Xhtml.append (List.map aux pl)

let to_html t =
  paragraph_list t

let to_string t =
  Xhtml.to_string (to_html t)

let assert_equal t1 t2 =
  failwith "TODO"

(* Default html creation functions for table of contents *)
module Toc = struct

  let a ~heading text =
    let href = "#" ^ id_of_heading heading in
    let link = Xhtml.({ href; text }) in
    Xhtml.List.li (Xhtml.link link)

  let ul ~depth l =
    if depth = 0 then
      Xhtml.div ["nav";"nav-list";"bs-docs-sidenav"] (Xhtml.List.ul l)
    else
      Xhtml.List.ul l

(*
  (* Extract a HTML table of contents from markdown elements. Depth can be
     modified with the corresponding optional argument. *)
  let to_html ?(wrap_list=wrap_ul) ?(wrap_item=wrap_a) ?(depth=2) ps =
    let rec aux level ps = match ps with
      | [] -> [], []
    | h :: t -> match h with
      | Heading (n,pt) ->
        begin
          match n with
          | n when n = level ->
              let acc, r = aux level t in
              wrap_item ~depth:level ~heading:h (par_text pt) :: acc, r
          | n when n > level && n <= depth ->
              let acc, r = aux (level+1) ps in
              let racc, rr = aux level r in
              wrap_list ~depth:level <:html< $list: acc$ >> :: racc, rr
          | n when n < level -> [], ps
          | _ -> aux level t
        end
      | _ -> aux level t
  in
  wrap_list ~depth:0 <:html< $list: fst (aux 1 ps)$ >>

*)

end
