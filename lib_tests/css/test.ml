let file =
  if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else
    "lib_tests/css/bootstrap.css"

let () =
  let read1 = Cow.Css.of_file file in
  let str1 = Cow.Css.to_string read1 in
  Printf.printf "CSS1(%s):\n%s\n" file str1;

  let read2 = Cow.Css.of_string str1 in
  let str2 = Cow.Css.to_string read2 in
  Printf.printf "CSS2(%s):\n%s\n" file str2;

  Cow.Css.assert_equal read1 read2


