let () =
  if Array.length Sys.argv <= 1 then (
    Printf.eprintf "usage: %s <filename.css>" (Filename.basename Sys.argv.(0));
    exit 1;
  )

let () =
  let read1 = Css.of_file Sys.argv.(1) in
  let str1 = Css.to_string read1 in
  Printf.printf "CSS1(%s):\n%s\n" Sys.argv.(1) str1;

  let read2 = Css.of_string str1 in
  let str2 = Css.to_string read2 in
  Printf.printf "CSS2(%s):\n%s\n" Sys.argv.(1) str2;

  Css.assert_equal read1 read2


